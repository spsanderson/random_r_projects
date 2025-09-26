library(healthyR.ai)
library(recipes)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

n <- 10L
l <- 5L
lo <- n * l

date_seq <- seq.Date(from = as.Date("2013-01-01"), length.out = lo, by = "month")
date_seq
val_seq <- replicate(n = l, c(runif(9), NA)) |> as.vector() |> as.double()
val_seq

data_tbl <- tibble(
  date_col = date_seq,
  value = val_seq
)

rec_obj <- recipe(value ~ date_col, data = data_tbl)
rec_obj

df_tbl <- tibble(
  impute_type = c("bagged","knn","linear","mean","median","roll"),
  rec_obj = list(rec_obj),
  data = list(data_tbl)
)
df_tbl[1,][[3]][[1]]

data_list <- df_tbl |>
  group_split(impute_type)

data_impute_list <- data_list |>
  imap(
    .f = function(obj, id){
      imp_type = obj |> pull(impute_type)
      rec_obj = obj |> pull(rec_obj) |> pluck(1)
      data = obj[["data"]][[1]]
      
      imp_obj <- hai_data_impute(
        .recipe_object = rec_obj,
        value,
        .type_of_imputation = imp_type,
        .roll_statistic = median
      )$impute_rec_obj

      imputed_data <- get_juiced_data(imp_obj)

      combined_tbl <- data |>
        left_join(imputed_data, by = "date_col") |>
        setNames(c("date_col", "original_value", "imputed_value")) |>
        mutate(rec_no = row_number()) |>
        mutate(color_col = original_value,
              size_col = original_value) |>
        mutate(impute_type = imp_type)
      
      return(combined_tbl)
    }
  )

combined_tbl <- data_impute_list |>
  list_rbind()

ggplot(data = combined_tbl,
  aes(
    x = date_col,
    y = imputed_value,
    color = color_col
    )
  ) + 
  facet_wrap(~ impute_type) +
  geom_point(data = combined_tbl |> filter(is.na(original_value)), aes(shape = 'NA', size = 3)) +
  scale_shape_manual(values = c('NA' = 3)) +
  geom_line(aes(x = date_col, y = original_value), color = "black") +
  geom_line(aes(x = date_col, y = imputed_value), color = "red", linetype = "dashed", alpha = .328) +
  geom_vline(
    data = combined_tbl[combined_tbl$original_value |> is.na(), ], 
    aes(xintercept = date_col), color = "black", linetype = "dashed"
  ) +
  labs(
    x = "Date",
    y = "Value",
    title = "Original vs. Imputed Data using HealthyR.ai",
    subtitle = "Function: hai_data_impute()",
    caption = "Red line is the imputed data, blue line is the original data"
  ) +
  theme_classic() +
  theme(legend.position = "none")

combined_tbl |>
  ggplot(aes(x = impute_type, y = imputed_value, color = color_col, group = impute_type)) +
  geom_boxplot() +
  labs(
    x = "Date",
    y = "Value",
    title = "Original vs. Imputed Data using HealthyR.ai",
    subtitle = "Function: hai_data_impute()"
  ) +
  theme_classic() +
  theme(legend.position = "none")
