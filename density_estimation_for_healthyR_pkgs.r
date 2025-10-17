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

      output$combined_data_tbl |>
        tidy_combined_autoplot() +
        labs(
          caption = pkg,
          title = paste0(
            "Density Plot for: ", pkg, "\n",
            "Parameters: .size = ", round(parameter_tbl$size, 3),
            ", .prob = ", round(parameter_tbl$prob, 3)
          )
        )
    }
  )
