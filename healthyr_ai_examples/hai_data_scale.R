library(healthyR.ai)
library(recipes)
library(dplyr)
library(ggplot2)
library(purrr)

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
  scale_type = c("center","normalize","range","scale"),
  rec_obj = list(rec_obj),
  data = list(data_tbl)
)
df_tbl[1,][[3]][[1]]

data_list <- df_tbl |>
  group_split(scale_type)

data_scale_list <- data_list |>
  imap(
    .f = function(obj, id){
      scale_type = obj[["scale_type"]]
      rec_obj = obj[["rec_obj"]][[1]]
      data = obj[["data"]][[1]]
      
      scale_obj <- hai_data_scale(
        .recipe_object = rec_obj,
        value,
        .type_of_scale = scale_type
      )$scale_rec_obj

      scaled_data <- get_juiced_data(scale_obj)

      combined_tbl <- data |>
        left_join(scaled_data, by = "date_col") |>
        setNames(c("date_col", "original_value", "scaled_value")) |>
        mutate(rec_no = row_number()) |>
        mutate(scale_type = scale_type)
      
      return(combined_tbl)
    }
  )

combined_tbl <- data_scale_list |>
  list_rbind()

ggplot(data = combined_tbl,
  aes(
    x = date_col,
    y = scaled_value
    )
  ) + 
  facet_wrap(~ scale_type) +
  geom_line(aes(x = date_col, y = original_value), color = "black") +
  geom_line(aes(x = date_col, y = scaled_value), color = "red", linetype = "dashed", alpha = .328) +
  labs(
    x = "Date",
    y = "Value",
    title = "Original vs. Scaled Data using HealthyR.ai",
    subtitle = "Function: hai_data_scale()",
    caption = "Red line is the scaled data, black line is the original data"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(data = combined_tbl,
  aes(
    x = date_col,
    y = scaled_value,
    group = scale_type,
    color = factor(scale_type)
    )
  ) + 
  geom_line(aes(x = date_col, y = scaled_value)) +
  labs(
    x = "Date",
    y = "Value",
    title = "Original vs. Scaled Data using HealthyR.ai",
    subtitle = "Function: hai_data_scale()",
    caption = "Red line is the scaled data, black line is the original data",
    color = "Color: Type of Scale"
  ) +
  theme_classic()

combined_tbl |>
  ggplot(
    aes(
      x = scale_type,
      y = scaled_value,
      group = scale_type,
      color = factor(scale_type)
    )) +
  geom_boxplot() +
  labs(
    x = "Date",
    y = "Value",
    title = "Original vs. Scaled Data using HealthyR.ai",
    subtitle = "Function: hai_data_scale()"
  ) +
  theme_classic() +
  theme(legend.position = "none")
