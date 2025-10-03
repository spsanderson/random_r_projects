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
val_seq <- replicate(n = l, rnorm(10)) |> as.vector() |> as.double()
val_seq

trig_trans <- c("sinh", "cosh", "tanh")
inv_fns <- c(TRUE, FALSE)

data_tbl <- tibble(
  date_col = date_seq,
  value = val_seq
)

rec_obj <- recipe(value ~ date_col, data = data_tbl)
rec_obj

df_tbl <- expand_grid(
    trig_type = trig_trans,
    inv_fn = inv_fns
  ) |>
  mutate(rec_obj = list(rec_obj)) |>
  mutate(data = list(data_tbl))

df_tbl[1,][[4]][[1]]

data_list <- df_tbl |>
  group_split(trig_type, inv_fn)

data_trans_list <- data_list |>
  imap(
    .f = function(obj, id){
      trig_type = obj |> pull(trig_type)
      inv_fn    = obj |> pull(inv_fn)
      rec_obj   = obj |> pull(rec_obj) |> pluck(1)
      data      = obj[["data"]][[1]]

      imp_obj <- hai_data_trig(
        .recipe_object = rec_obj,
        value,
        .type_of_scale = trig_type,
        .inverse       = inv_fn
      )$scale_rec_obj

      imputed_data <- get_juiced_data(imp_obj)

      combined_tbl <- data |>
        left_join(imputed_data, by = "date_col") |>
        setNames(c("date_col", "original_value", "trig_value")) |>
        mutate(rec_no = row_number()) |>
        mutate(trig_type = trig_type) |>
        mutate(inv_fn = inv_fn) |>
        mutate(trans_type = toupper(paste0(trig_type, " ", inv_fn)))
      
      return(combined_tbl)
    }
  )

combined_tbl <- data_trans_list |>
  list_rbind() |>
  bind_rows(
    data_tbl |>
      rename("date_col" = "date_col", "original_value" = "value") |>
      mutate(
        trig_value = NA_real_,
        rec_no = 1:lo,
        trig_type = "Original",
        inv_fn = FALSE,
        trans_type = "Original"
      )
  )

combined_factor_tbl <- combined_tbl|>
  mutate(
    trans_type = factor(
      trans_type, 
      levels = summarize(combined_tbl, mean_val = mean(trig_value), .by = trans_type) |> arrange(desc(mean_val)) |>
        pull(trans_type)
    )
  )

combined_factor_tbl |>
  ggplot(aes(x = date_col, y = trig_value, group = trans_type)) +
  geom_point(aes(color = trans_type), size = 2, alpha = 0.6) +
  geom_line(aes(color = trans_type), linewidth = 1, alpha = 0.6) +
#  facet_wrap(~ trans_type, scales = "free_y") +
  labs(
    title = "Data with Trig Transformation's using healthyR.ai",
    x = "Date",
    y = "Transformed Value",
    color = "trans_type"
  ) +
  theme_minimal()

combined_factor_tbl |>
  ## facet box plots of the value column by name column
  ggplot(aes(x = trans_type, y = trig_value, group = trans_type)) +
  geom_boxplot(aes(color = trans_type, group = trans_type)) +
  geom_boxplot(data = combined_tbl |>
    filter(trans_type == "Original"),
    aes(x = trans_type, y = original_value, group = trans_type)) +
  geom_jitter(aes(color = trans_type, group = trans_type), width = 0.3, alpha = .3) +
  labs(
    x = "Trig Type",
    y = "Value",
    title = "Original vs. Transformed Data using HealthyR.ai",
    subtitle = "Various Data trans_types (Hyperbolic sin, cos and tan) and their inverses."
  ) +
  theme_classic() +
  theme(legend.position = "none")  +
  theme(
    # rotate x axis labels
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
