library(TidyDensity)
library(dplyr)
library(ggplot2)
library(purrr)
library(tictoc)
library(rbenchmark)

x <- mtcars$mpg
n     <- length(x)
times <- 2000L
prop  <- .8
size  <- floor(n * prop)

benchmark("TidyDensity" = {
  te <- tidy_bootstrap(.x = x, .num_sims = times, .proportion = prop) %>%
    bootstrap_unnest_tbl() 
},
"PieceWise_Purr" = {
  idx <- map(rep(n, times), sample, size = size, replace = TRUE)
  l   <- map(idx, ~ x[.x] %>% as_tibble())
  ldf <- imap(l, ~ bind_cols(.x, sim_number = factor(.y))) %>%
    map_df(as_tibble)
},
"Modified_Single_Purrr" = {
  df_tbl <- map(rep(n, times), sample, size = size, replace = TRUE) %>%
    map(~ x[.x]) %>%
    imap(~ cbind(.x, sim_number = factor(.y))) %>%
    map_df(as_tibble)
},
replications = 1000,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

tic()
te <- tidy_bootstrap(.x = x, .num_sims = times, .proportion = prop) %>%
  bootstrap_unnest_tbl() 
toc()

tic()
idx <- map(rep(n, times), sample, size = size, replace = TRUE)
l   <- map(idx, ~ x[.x] %>% as_tibble())
ldf <- imap(l, ~ bind_cols(.x, sim_number = factor(.y))) %>%
  map_df(as_tibble)
toc()

tic()
df_tbl <- map(rep(n, times), sample, size = size, replace = TRUE) %>%
  map(~ x[.x]) %>%
  imap(~ cbind(.x, sim_number = factor(.y))) %>%
  map_df(as_tibble)
toc()

te %>%
  ggplot(aes(y = y, color = sim_number)) +
  geom_boxplot() +
  theme_minimal()

ldf %>%
  ggplot(aes(y = value, color = sim_number)) +
  geom_boxplot() +
  theme_minimal()

df_tbl %>%
  ggplot(aes(y = y, color = sim_number)) +
  geom_boxplot() +
  theme_minimal()