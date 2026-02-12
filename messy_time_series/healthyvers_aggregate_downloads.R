library(tidyverse)
library(timetk)

url <- "https://raw.githubusercontent.com/spsanderson/package-downloads/master/"
f_name <- "old_downloads.RDS"
f_url <- paste0(url, f_name)
data <- readRDS(url(f_url, method = "libcurl"))
data <- as_tibble(data)

df <- data |>
  select(package, date) |>
  summarize_by_time(
    .date_var = date,
    .by = "day",
    n = n()
  ) |>
ungroup() |>
arrange(date)

write_csv(df, "cran_logs.csv")
