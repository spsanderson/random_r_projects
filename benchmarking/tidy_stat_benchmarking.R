library(rbenchmark)
library(TidyDensity)
library(dplyr)

# Get the interesting vector, well for this anyways
x <- mtcars$mpg

# Bootstrap the vector (2k simulations is default)
tb <- tidy_bootstrap(x) %>%
  bootstrap_unnest_tbl()

benchmark(
  "tibble" = {
    t <- tidy_stat_tbl(tb, y, IQR, "tibble")
  },
  "data.table" = {
    d <- tidy_stat_tbl(tb, y, IQR, .use_data_table = TRUE, type = 7)
  },
  "sapply" = {
    s <- tidy_stat_tbl(tb, y, IQR, "vector")
  },
  "lapply" = {
    l <- tidy_stat_tbl(tb, y, IQR, "list")
  },
  replications = 25,
  columns = c("test","replications","elapsed","relative","user.self","sys.self")
) %>%
  arrange(relative)
