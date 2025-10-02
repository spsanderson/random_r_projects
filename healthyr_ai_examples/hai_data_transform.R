library(healthyR.ai)
library(recipes)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(patchwork)

n <- 10L
l <- 5L
lo <- n * l

date_seq <- seq.Date(from = as.Date("2013-01-01"), length.out = lo, by = "month")
date_seq
val_seq <- replicate(n = l, sample(1:10, n)) |> as.vector() |> as.double()
val_seq

data_tbl <- tibble(
  date_col = date_seq,
  value = val_seq
)
head(data_tbl)

rec_obj <- recipe(value ~ date_col, data = data_tbl)
rec_obj

box_cox_df <- hai_data_transform(
    .recipe_object = rec_obj,
    value,
    .type_of_scale = "boxcox"
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "boxcox")

df_train <- sample_n(data_tbl, size = .8 * nrow(data_tbl))
df_test <- anti_join(data_tbl, df_train, by = "date_col")
bs_rec_obj <- recipe(value ~ date_col, data = df_train)
bs_df <- hai_data_transform(
    .recipe_object = bs_rec_obj,
    value,
    .type_of_scale = "bs",
    .bs_deg_free = 1,
    .bs_degree = 1
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "bs") %>%
  setNames(c("date_col", "value", "transformation"))

log_tbl <- hai_data_transform(
    .recipe_object = rec_obj,
    value,
    .type_of_scale = "log"
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "log")

ns_tbl <- hai_data_transform(
    .recipe_object = rec_obj,
    value,
    .type_of_scale = "ns",
    .ns_deg_free = 1
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "ns") %>%
  setNames(c("date_col", "value", "transformation"))

relu_tbl <- hai_data_transform(
    .recipe_object = rec_obj,
    value,
    .type_of_scale = "relu",
    .rel_smooth = TRUE
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "relu") |>
  select(- value) |>
  setNames(c("date_col", "value", "transformation"))

sqrt_tbl <- hai_data_transform(
    .recipe_object = rec_obj,
    value,
    .type_of_scale = "sqrt"
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "sqrt")

yeojohnson_tbl <- hai_data_transform(
    .recipe_object = rec_obj,
    value,
    .type_of_scale = "yeojohnson"
  )$scale_rec_obj %>%
    get_juiced_data() %>%
    mutate(transformation = "yeojohnson")

combined_tbl <- bind_rows(
  box_cox_df,
  bs_df,
  log_tbl,
  ns_tbl,
  relu_tbl,
  sqrt_tbl,
  yeojohnson_tbl,
  data_tbl %>% mutate(transformation = "original_value") 
) |>
  mutate(
    transformation = factor(
      transformation, 
      levels = summarize(combined_tbl, mean_val = mean(value), .by = transformation) |> arrange(desc(mean_val)) |>
        pull(transformation)
    )
  )

combined_tbl |>
  ggplot(aes(x = date_col, y = value, group = transformation)) +
  geom_point(aes(color = transformation), size = 2, alpha = 0.6) +
  geom_line(aes(color = transformation), linewidth = 1, alpha = 0.6) +
#  facet_wrap(~ transformation, scales = "free_y") +
  labs(
    title = "Data Transformations using healthyR.ai",
    x = "Date",
    y = "Transformed Value",
    color = "Transformation"
  ) +
  theme_minimal()

combined_tbl |>
  ## facet box plots of the value column by name column
  ggplot(aes(x = transformation, y = value, group = transformation)) +
  geom_boxplot(aes(color = transformation, group = transformation)) +
  geom_jitter(aes(color = transformation, group = transformation), width = 0.3, alpha = .3) +
  labs(
    x = "Transformation",
    y = "Value",
    title = "Original vs. Transformed Data using HealthyR.ai",
    subtitle = "Various Data Transformations (Box-Cox, Yeo-Johnson, Log, Sqrt, B-Spline, Natural Spline, ReLU"
  ) +
  theme_classic() +
  theme(legend.position = "none")  +
  theme(
    # rotate x axis labels
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
