library(TidyDensity)
library(tidyverse)
library(rlang)
library(timetk)

url <- "https://raw.githubusercontent.com/spsanderson/package-downloads/master/"
f_name <- "old_downloads.RDS"
f_url <- paste0(url, f_name)
data_tbl <- as_tibble(readRDS(url(f_url, method = "libcurl")))
head(data_tbl)

clean_data_tbl <- data_tbl |>
  mutate(version = str_extract(version, "\\d+\\.\\d+\\.\\d+"))
head(clean_data_tbl)

dirty_data_tbl <- data_tbl |>
  anti_join(clean_data_tbl)
head(dirty_data_tbl)

dirty_data_tbl |>
  count(package, version, sort = TRUE)

inform(
  message = paste0("Total rows removed for bad version number: ", nrow(data_tbl) - nrow(clean_data_tbl)),
  use_cli_format = TRUE
)

global_count_vec <- summarise_by_time(
  .data = clean_data_tbl,
  .date_var = date,
  .by = "day",
  value = n()
) |>
  pad_by_time(
    .date_var = date,
    .by = "day", 
    .pad_value = 0
  ) |>
  pull(value)

util_poisson_param_estimate(global_count_vec)$combined_data_tbl |>
  tidy_combined_autoplot()

util_negative_binomial_param_estimate(global_count_vec)$combined_data_tbl |>
  tidy_combined_autoplot()

global_neg_bin_output <- util_negative_binomial_param_estimate(global_count_vec)
global_neg_bin_output

package_count_tbl <- summarise_by_time(
  .data = clean_data_tbl |> group_by(package),
  .date_var = date,
  .by = "day",
  value = n()
) |>
  pad_by_time(
    .date_var = date,
    .by = "day", 
    .pad_value = 0
  ) |>
  ungroup()

package_count_tbl |>
  group_split(package) |>
  imap(
    .f = function(obj, id){
      df <- obj
      x <- obj[["value"]]
      pkg <- obj[["package"]]

      output <- util_negative_binomial_param_estimate(x)
      parameter_tbl <- output$parameter_tbl |>
        filter(method == "MLE_Optim") |>
        select(samp_size, size, prob)
      x_aic <- util_negative_binomial_aic(x)

      output$combined_data_tbl |>
        tidy_combined_autoplot() +
        labs(
          caption = pkg,
          title = paste0(
            "Density Plot for: ", pkg, "\n",
            "Parameters: .size = ", round(parameter_tbl$size, 3),
            ", .prob = ", round(parameter_tbl$prob, 3),
            ", AIC = ", round(x_aic, 0)
          )
        )
    }
  )

package_count_tbl |>
  group_split(package) |>
  imap(
    .f = function(obj, id){
      x <- obj[["value"]]
      pgk <- obj[["package"]][[1]]

      output <- util_negative_binomial_param_estimate(x)$parameter_tbl |>
        filter(method == "MLE_Optim") |>
        select(samp_size, size, prob)
      x_aic <- util_negative_binomial_aic(x)
      tidy_dist <- tidy_negative_binomial(.n = length(x), .size = output$size, .prob = output$prob)
      dist_summ <- tidy_distribution_summary_tbl(tidy_dist)

      res <- tibble(
        package = pgk,
        size = output$size,
        prob = output$prob,
        aic = x_aic,
        dist_summary = dist_summ
      ) |>
        unnest(dist_summary)

      return(res)
    }
  ) |>
  list_rbind()

fns_tbl <- tibble(fns = ls.str("package:TidyDensity")) |>
  filter(grepl("_aic", fns)) |>
  mutate(params = purrr::map(fns, formalArgs)) |> 
  group_by(fns) |> 
  mutate(func_with_params = toString(params)) |>
  mutate(
    func_with_params = ifelse(
      str_detect(
        func_with_params, "\\("), 
      paste0(fns, func_with_params), 
      paste0(fns, "(", func_with_params, ")")
    )) |>
  select(fns, func_with_params) |>
  mutate(x = list(global_count_vec))

global_aic <- fns_tbl |>
  rowwise() |>
  mutate(
    aic = {
      result <- try(get(fns)(x), silent = TRUE)
      ifelse(inherits(result, "try-error"), NA_real_, result)
    }
  )

global_aic |> 
  drop_na() |>
  arrange(aic)
