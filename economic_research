library(tidyverse)
library(RcppRoll)
library(patchwork)
library(seasonal)

n <- 365L * 1
end_date <- Sys.Date()
date_vec <- seq.Date(to = end_date, by = "days", length.out = n)
start_date <- min(date_vec)
sy <- year(start_date)
sm <- month(start_date)
ey <- year(end_date)
em <- month(end_date)

x <- rnorm(n = n) |> cumsum()

x_ts <- ts(data = x, start = c(sy, sm), end = c(ey, em), frequency = 365.25)
attributes(x_ts)

df_tbl <- tibble(
  date_col = date_vec,
  value = x
)

df_mutated_tbl <- df_tbl |>
  arrange(date_col) |>
  mutate(
    value_7dma = roll_meanr(
      x = value,
      n = 7,
      na.rm = TRUE
    
    )
  ) |>
  mutate(
    value_12m = (roll_prodr(1 + (value / 100), n = 12) -1) * 100
  ) |>
  mutate(
    value_level = cumprod(1 + (value / 100)),
    value_level = (value_level / first(value_level)) * 100
  )

p1 <- df_mutated_tbl |>
  ggplot(aes(x = date_col)) +
  geom_line(aes(y = value, color = "Original Value")) +
  geom_line(aes(y = value_7dma, color = "7 Day Moving Average")) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(
    x = "",
    y = "",
    title = "7 Day Moving Average",
    color = ""
  )

p2 <- df_mutated_tbl |>
  ggplot(aes(x = date_col)) +
  geom_line(aes(y = value_12m, color = "12 Month Accumulation")) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(
    x = "",
    y = "",
    title = "12 Month Accumulation",
    color = ""
  )

p3 <- df_mutated_tbl |>
  ggplot(aes(x = date_col)) +
  geom_line(aes(y = value_level, color = "Level")) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(
    x = "",
    y = "",
    title = "Value Level",
    color = ""
  )

p1 + (p2 / p3)
r