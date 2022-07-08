library(healthyverse)
library(tidyverse)

rw <- ts_random_walk()

rw %>%
  ggplot(aes(x = x, y = cum_y, group = factor(run), color = factor(run))) +
  geom_line() +
  ts_random_walk_ggplot_layers(rw)

df <- ts_to_tbl(AirPassengers) %>%
  select(-index)

event_tbl <- ts_time_event_analysis_tbl(
  .data = df,
  .date_col = date_col,
  .value_col = value,
  .horizon = 6,
  .direction = "both"
)

df_tbl <- event_tbl %>%
  select(x, median_event_change, event_change_ci_low, event_change_ci_high) %>%
  distinct()
  
df_tbl %>%
  ggplot(aes(x = x, y = median_event_change)) +
  geom_line() +
  geom_line(aes(y = event_change_ci_high), color = "red", linetype = "dashed") +
  geom_line(aes(y = event_change_ci_low), color = "red", linetype = "dashed") +
  geom_vline(
    xintercept = c(1, which(df_tbl$median_event_change == 0), nrow(df_tbl)), 
    color = "steelblue", 
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
    x = "Horizon Event"
  )
  