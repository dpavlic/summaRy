#' @importFrom dplyr arrange case_when collect count filter  mutate mutate_at
#'                   pull rename  select  summarize
#' @importFrom utils head
#' @importFrom rlang enquos eval_tidy expr
#' @importFrom purrr map map2
#' @importFrom magrittr `%>%`

.lookup_type <- function(df, var) {
  df %>%
    select({{ var }}) %>%
    head(100) %>%
    pull()
}

.calculate_univariate <- function (x, ...) {
  UseMethod(".calculate_univariate", x)
}

.calculate_univariate.character <- function(x, ...) {
  dots <- enquos(...)
  df <- eval_tidy(dots[[1]])
  var <- dots[[2]]

  count_var <- df %>%
    select({{ var }}) %>%
    filter(!is.na({{ var }}))

  ntotal <- count(count_var) %>%
    pull() %>%
    as.double()

  df_stat <- count_var %>%
    count({{ var }}) %>%
    rename(count = n) %>%
    mutate(freq = count / ntotal) %>%
    arrange(desc(freq)) %>%
    head(10) %>%
    collect() %>%
    mutate(
      freq = round(freq * 100, 2),
      count = as.double(count)
    ) %>%
    rename(label = {{ var }})

  top_x_count <- summarize(df_stat, n = sum(count)) %>% pull

  diffs <- tibble(
    label = c('Top 10', 'All Others'),
    count =  c(top_x_count, ntotal - top_x_count)
  ) %>%
    mutate(freq = round((count / ntotal) * 100, 2))

  # Find out the missing frequencies
  missing_count <- df %>%
    select({{ var }}) %>%
    filter(is.na({{ var }})) %>%
    summarize(n = n()) %>%
    pull()

  total_obs_count <- summarize(df, n = n()) %>% pull()
  missings <- tibble(
    label = c('Missing', 'Non-Missing'),
    count = as.double(c(missing_count, total_obs_count))
  ) %>%
    mutate(freq = round((count / sum(count)) * 100, 2))

  list(
    top_10 = df_stat,
    others = diffs,
    missing = missings
  )
}

.calculate_univariate.POSIXct <- .calculate_univariate.POSIXt <- function(x, ...) {
  dots <- enquos(...)
  df <- eval_tidy(dots[[1]])
  var <- dots[[2]]

  ptls <- c(seq(0.1, 0.9, by = 0.1), .25, .75)
  quantile_exprs <- map(ptls, ~ expr(quantile(epoch, c(!!.x)))) %>%
    set_names(paste0('p', ptls * 100))

  df <- df %>%
    select({{ var }}) %>%
    mutate(epoch = EXTRACT(epoch %from% {{ var }}))

  df_summary <- df %>%
    summarize(
      min = min(epoch, na.rm = TRUE),
      max = max(epoch, na.rm = TRUE),
      mean = mean(epoch),
      !!!quantile_exprs
    ) %>%
    collect()

  bins <- seq(as.numeric(df_summary$min), as.numeric(df_summary$max)) %>%
    quantile(probs = c(seq(0.1, 0.9, by = 0.1))) %>%
    sort(decreasing = TRUE) %>%
    unname()
  case_expr <- map2(
    bins,
    sort(seq_along(bins), decreasing = TRUE),
    ~ expr(epoch > !!.x ~ !!.y)
  )

  df_rank <- mutate(df, epoch_rank = case_when(!!!case_expr, TRUE ~ 0)) %>%
    count(epoch_rank) %>%
    collect()

  df_summary_date_normal <- df_summary %>%
    mutate_at(vars(min, max, mean, starts_with('p')), anytime::anytime) %>%
    mutate(iqr = p75 - p25) %>%
    t()

  list(
    stats = df_summary_date_normal,
    hist = df_rank
  )
}

#' @export
calculate_univariate <- function(df, var) {
  sample <- .lookup_type(df, {{ var }})
  .calculate_univariate(sample, df, {{ var }})
}
