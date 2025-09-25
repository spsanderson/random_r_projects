library(healthyR.ai)
library(recipes)
library(dplyr)
library(ggplot2)

n <- 10L
l <- 3L
lo <- n * l

date_seq <- seq.Date(from = as.Date("2013-01-01"), length.out = lo, by = "month")
date_seq
val_seq <- replicate(n = l, c(rnorm(9), NA)) |> as.vector() |> as.double()
val_seq

df_tbl <- tibble(
  date_col = date_seq,
  value = val_seq
)

rec_obj <- recipe(value ~ date_col, data = df_tbl)
rec_obj

imp_obj <- hai_data_impute(
  .recipe_object = rec_obj,
  value,
  .type_of_imputation = "bagged",
  .roll_statistic = median
)$impute_rec_obj
imputed_data <- get_juiced_data(imp_obj)

combined_tbl <- df_tbl |>
  left_join(imputed_data, by = "date_col") |>
  setNames(c("date_col", "original_value", "imputed_value")) |>
  mutate(rec_no = row_number()) |>
  mutate(color_col = original_value,
        size_col = original_value)

ggplot(data = combined_tbl,
  aes(
    x = date_col,
    y = imputed_value,
    color = color_col
    )
  ) + 
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
