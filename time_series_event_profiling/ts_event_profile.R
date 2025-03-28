# time to event analysis
# time to event analysis
ts_time_event_analysis_tbl <- function(.data, .date_col, .value_col,
                                       .percent_change = 0.05, .horizon = 12,
                                       .precision = 2, .direction = "forward",
                                       .filter_non_event_groups = TRUE) {
  
  # Tidyeval ----
  date_var_expr <- rlang::enquo(.date_col)
  value_var_expr <- rlang::enquo(.value_col)
  horizon <- as.integer(.horizon)
  precision <- as.numeric(.precision)
  percent_change <- as.numeric(.percent_change)
  direction <- tolower(as.character(.direction))
  filter_non_event_groups <- as.logical(.filter_non_event_groups)
  
  change_sign <- ifelse(percent_change < 0, -1, 1)
  
  # Checks ----
  if (rlang::quo_is_missing(date_var_expr)) {
    rlang::abort(
      message = "You must supply a date column.",
      use_cli_format = TRUE
    )
  }
  
  if (rlang::quo_is_missing(value_var_expr)) {
    rlang::abort(
      message = "You must supply a value column.",
      use_cli_format = TRUE
    )
  }
  
  if (!is.integer(horizon) | horizon < 1) {
    rlang::abort(
      message = "The '.horizon' parameter must be and integer of 1 or more.",
      use_cli_format = TRUE
    )
  }
  
  if (percent_change < -1 | percent_change > 1 | !is.numeric(percent_change)) {
    rlang::abort(
      message = "The '.percent_change' parameter must be a numeric/dbl and between
      -1 and 1 inclusive.",
      use_cli_format = TRUE
    )
  }
  
  if (!is.numeric(precision) | precision < 0) {
    rlang::abort(
      message = "The '.precision' parameter must be a numeric/dbl and must be
      greater than or equal to 0.",
      use_cli_format = TRUE
    )
  }
  
  if (!is.character(direction) | !direction %in% c("backward", "forward", "both")) {
    rlang::abort(
      message = "The '.direction' parameter must be a character string of either
      'backward', 'forward', or 'both'.",
      use_cli_format = TRUE
    )
  }
  
  # Data ----
  df <- dplyr::as_tibble(.data) %>%
    dplyr::select({{ date_var_expr }}, {{ value_var_expr }}, dplyr::everything()) %>%
    dplyr::arrange({{ date_var_expr }}) %>%
    # Manipulation
    dplyr::mutate(
      lag_val = dplyr::lag({{ value_var_expr }}, 1),
      adj_diff = ({{ value_var_expr }} - lag_val),
      relative_change_raw = adj_diff / lag_val
    ) %>%
    tidyr::drop_na(lag_val) %>%
    dplyr::mutate(
      relative_change = round(relative_change_raw, precision),
      pct_chg_mark = ifelse(relative_change == percent_change, TRUE, FALSE),
      event_base_change = ifelse(pct_chg_mark == TRUE, 0, relative_change_raw),
      group_number = cumsum(pct_chg_mark)
    ) %>%
    dplyr::mutate(numeric_group_number = group_number) %>%
    dplyr::mutate(group_number = as.factor(group_number))
  
  # Drop group 0 if indicated
  if (filter_non_event_groups){
    df <- df %>%
      dplyr::filter(numeric_group_number != 0)
  }
  
  if (direction == "forward"){
      df_final_tbl <- internal_ts_forward_event_tbl(df, horizon)
  }
  
  if (direction == "backward"){
      df_final_tbl <- internal_ts_backward_event_tbl(df, horizon)
  }
  # df <- df %>%
  #   dplyr::group_by(group_number) %>%
  #   dplyr::mutate(x = dplyr::row_number()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(x) %>%
  #   dplyr::mutate(
  #     mean_event_change = mean(event_base_change, na.rm = TRUE),
  #     median_event_change = stats::median(event_base_change, na.rm = TRUE),
  #     event_change_ci_low = unname(stats::quantile(event_base_change, 0.025, na.rm = TRUE)),
  #     event_change_ci_high = unname(stats::quantile(event_base_change, 0.975, na.rm = TRUE))
  #   ) %>%
  #   dplyr::ungroup()
  # 
  # df_final_tbl <- df %>%
  #   dplyr::group_by(group_number) %>%
  #   dplyr::slice(1:horizon) %>%
  #   dplyr::ungroup()
  
  max_event_change <- max(df_final_tbl$event_base_change)
  max_groups <- max(df_final_tbl$numeric_group_number)
  
  # Output ----
  attr(df_final_tbl, ".change_sign") <- change_sign
  attr(df_final_tbl, ".percent_change") <- .percent_change
  attr(df_final_tbl, ".horizon") <- .horizon
  attr(df_final_tbl, ".precision") <- .precision
  attr(df_final_tbl, ".direction") <- .direction
  attr(df_final_tbl, ".filter_non_event_groups") <- .filter_non_event_groups
  attr(df_final_tbl, ".max_event_change") <- max_event_change
  attr(df_final_tbl, ".max_group_number") <- max_groups
  
  return(df_final_tbl)
}

library(healthyverse)
library(tidyverse)
library(timetk)
library(scales)

set.seed(123)
tn <- tidy_multi_single_dist(
  .tidy_dist = "tidy_normal",
  .param_list = list(
    .n = 200,
    .mean = 0,
    .sd = c(0.01, 0.05, 0.1, 0.2),
    .num_sims = 1
  )
)

time_vec <- tk_make_timeseries(
  start_date = "2021",
  by = "day",
  length_out = 200
)

td <- tn %>%
  select(dist_name, x, y) %>%
  group_by(dist_name) %>%
  mutate(date_col = time_vec) %>%
  mutate(rw_val = cumsum(y)) %>%
  mutate(group_vol = sql_mid(as.character(dist_name), 15, 4)) %>%
  mutate(group_vol = str_remove_all(group_vol, "\\)")) %>%
  mutate(group_name = paste0(
    "Volatility of: ",
    scales::percent(as.numeric(group_vol))
  ) %>%
    as_factor()) %>%
  ungroup() %>%
  select(date_col, dist_name, everything())

td %>%
  ggplot(aes(x = x, y = rw_val)) +
  geom_line() +
  facet_wrap(~ group_name, scales = "free") +
  theme_minimal() +
  labs(
    title = "Normaly Distributed Random Walk - 200 observations",
    subtitle = "The .sd argument set to 0.01, 0.05, etc.",
    y = "Random Walk Value",
    x = "Time Step"
  )

td_event_tbl <- td %>% 
  group_split(dist_name) %>%
  map(~ ts_time_event_analysis_tbl(
    .data = .x,
    .date_col = date_col,
    .value_col = rw_val,
    .horizon = 30,
    .direction = "backward"
  )) %>%
  map_df(as_tibble)

td_event_tbl %>%
  ggplot(aes(
    x = x, 
    y = mean_event_change
  )) +
  geom_line() +
  geom_line(aes(y = event_change_ci_high), color = "red", linetype = "dashed") +
  geom_line(aes(y = event_change_ci_low), color = "red", linetype = "dashed") +
  facet_wrap(~ group_name, scales = "free") +
  theme_minimal() +
  labs(
    title = paste0(
      "Event Analysis at ", 
      attributes(td_event_tbl)$.percent_change %>% scales::percent(),
      " on a ",
      attributes(td_event_tbl)$.direction,
      " basis."
    ),
    y = "Event Base % Change",
    x = "",
    color = "Group Number"
  ) +
  theme(legend.position = "bottom")

td_event_tbl %>%
  ggplot(aes(x = x, y = event_base_change, group = group_number, color = group_number)) +
  geom_line() +
  geom_line(aes(y = mean_event_change), color = "black", linethype = "dashed") +
  facet_wrap(~ group_name, scales = "free") +
  theme_minimal()


