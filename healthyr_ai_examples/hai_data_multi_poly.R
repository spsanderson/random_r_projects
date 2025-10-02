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
val_seq <- replicate(n = l, rnorm(10, 4, 5)) |> as.vector() |> as.double()
val_seq

data_tbl <- tibble(
  date_col = date_seq,
  value = val_seq
)

rec_obj <- recipe(value ~ date_col, data = data_tbl)
rec_obj

df_tbl <- tibble(
  poly_n = 1:4,
  rec_obj = list(rec_obj),
  data = list(data_tbl)
)
df_tbl[1,][[3]][[1]]

data_list <- df_tbl |>
  group_split(poly_n)

data_poly_list <- data_list |>
  imap(
    .f = function(obj, id){
      poly_num = obj[["poly_n"]]
      rec_obj = obj[["rec_obj"]][[1]]
      data = obj[["data"]][[1]]
      
      poly_obj <- hai_data_poly(
        .recipe_object = rec_obj,
        value,
        .p_degree = poly_num
      )$scale_rec_obj

      poly_data <- get_juiced_data(poly_obj)

      combined_tbl <- data |>
        left_join(poly_data, by = "date_col") |>
        rename("original_value" = value) |>
        mutate(rec_no = row_number()) |>
        mutate(poly_n = poly_num) |>
        select(date_col, rec_no, original_value, poly_n, everything())
      
      return(combined_tbl)
    }
  )

combined_tbl <- data_poly_list |>
  list_rbind()

p1 <- combined_tbl |>
  pivot_longer(contains("value_")) |>
  drop_na() |>
  mutate(facet_text = paste0("Polynomial Degree: ", poly_n)) |>
  ## facet box plots of the value column by name column
  ggplot(aes(x = name, y = value, group = name)) +
  facet_wrap(~ facet_text, scales = "free") +
  geom_boxplot(aes(color = name, group = name)) +
  geom_jitter(aes(color = name, group = name), width = 0.3, alpha = .3) +
  labs(
    x = "Polynomial Degree",
    y = "Value",
    title = "Original vs. Poly Data using HealthyR.ai",
    subtitle = "Function: hai_data_poly()",
    caption = "Degrees 1 to 4"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(
    # rotate x axis labels
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p2 <- combined_tbl |>
  select(original_value, poly_n) |>
  mutate(name = "Original Data") |>
  ggplot(aes(x = name, y = original_value, group = name)) +
  geom_boxplot() +
  geom_jitter(aes(x = name, y = original_value), width = 0.3, alpha = .3, color = "blue") +
  theme_classic() +
  labs(
    x = "",
    y = "Value",
    title = "Original Value Distribution"
  ) +
  theme(
    # rotate x axis labels
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p1 + p2
