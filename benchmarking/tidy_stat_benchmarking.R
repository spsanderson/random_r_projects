library(rbenchmark)
library(data.table)
library(TidyDensity)
library(dplyr)

# The function
func <- IQR
func_chr <- deparse(substitute(IQR))

# Get the interesting vector, well for this anyways
x <- mtcars$mpg

# Bootstrap the vector (2k simulations is default)
tb <- tidy_bootstrap(x) %>%
  bootstrap_unnest_tbl()

# Make a list object splitting on sim_number
df_tbl <- tb %>% 
  split(.$sim_number) %>%
  map(~ .x %>% pull(y))

# Make a data.table object
tbd <-  tb %>%
  as.data.table()

benchmark(
  "tibble" = {
    t <- tidy_stat_tbl(tb, y, IQR, "tibble")
  },
  "new_func" = {
    d <- melt(
      tbd[, as.list(func(y)), sim_number],  
      id.var = "sim_number",  
      value.name = func_chr
    ) %>%
      as_tibble() %>%
      rename(name = variable) %>%
      arrange(sim_number, name)
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
