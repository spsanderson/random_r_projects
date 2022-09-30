library(TidyDensity)
library(dplyr)
library(rbenchmark)

x <- mtcars$mpg

te <- tidy_bootstrap(x) %>%
  bootstrap_unnest_tbl()

benchmark(
  "tibble" = {
    t <- tidy_stat_tbl(te, y, IQR, "tibble")
  },
  "tibble2" = {
    t2 <- tidy_stat_tbl2(te,y,IQR,"tibble")
  },
  "sapply" = {
    s <- tidy_stat_tbl(te, y, IQR, "vector")
  },
  "lapply" = {
    l <- tidy_stat_tbl(te, y, IQR, "list")
  },
  replications = 25,
  columns = c("test","replications","elapsed","relative","user.self","sys.self")
) %>%
  arrange(relative)
