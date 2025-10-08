library(TidyDensity)
library(RandomWalker)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

tri_dist <- function() {
  tidy_triangular(.n = 2, )[["y"]][[1]]
}

walk_data <- custom_walk(.custom_fns = tri_dist, .dimensions = 2, .num_walks = 4)
  
walk_data |>
  ggplot(aes(x = cum_min_x, y = cum_min_y, group = walk_number, color = step_number)) +
  facet_wrap(~ walk_number) +
  geom_path(alpha = .5) +
  scale_color_viridis_c(option = "plasma") +
  geom_point(data = walk_data |>
    group_by(walk_number) |>
    filter(step_number == min(step_number) | step_number == max(step_number)) |>
    ungroup(),
    aes(color = step_number), size = 3
  ) +
  theme_classic() +
  labs(
    color = "Step Number",
    y = 'Cumulative Minimum Y',
    x = 'Cumulative Minimum X'
  )

walk_data |>
  ggplot(aes(x = cum_min_x, y = cum_min_y, group = walk_number, color = walk_number)) +
  geom_path(alpha = .5) +
  geom_point(data = walk_data |>
    group_by(walk_number) |>
    filter(step_number == min(step_number) | step_number == max(step_number)) |>
    ungroup(),
    aes(color = walk_number), size = 3
  ) +
  theme_classic() +
  labs(
    color = "Walk Number",
    y = 'Cumulative Minimum Y',
    x = 'Cumulative Minimum X'
  )

walk_data |>
  ggplot(aes(x = x, y = y, group = walk_number, color = step_number)) +
  facet_wrap(~ walk_number, scales = "free") +
  geom_path(alpha = .5) +
  scale_color_viridis_c(option = "plasma") +
  geom_point(data = walk_data |>
    group_by(walk_number) |>
    filter(step_number == min(step_number) | step_number == max(step_number)) |>
    ungroup(),
    aes(color = step_number), size = 3
  ) +
  theme_classic() +
  labs(
    color = "Step Number",
    y = 'Y',
    x = 'X'
  )

walk_data |>
  group_split(walk_number) |>
  imap(
    .f = function(obj, id) {
      df <- obj
      vec_dist <- euclidean_distance(df, x, y, TRUE) |> mean(na.rm = TRUE)
      return(vec_dist)
    }
  ) |>
  tibble::enframe() |>
  rename(walk_number = name, mean_dist = value) |>
  unnest(mean_dist)

walk_data |>
  select(walk_number, x, y) |>
  group_nest(walk_number) |>
  mutate(
    mean_dist = map(
      data, 
      \(data) euclidean_distance(data, x, y, TRUE) |>
        mean(na.rm = TRUE)
    )
  ) |>
  select(-data) |>
  unnest(mean_dist)

walk_data |>
  filter(step_number == min(step_number) | step_number == max(step_number)) |>
  group_nest(walk_number) |>
  mutate(
    euc_dist = map(
      data, 
      \(data) euclidean_distance(data, x, y, TRUE)
    )
  ) |>
  select(-data) |>
  unnest(euc_dist) |>
  drop_na() |>
  mutate(
    dist_text = glue::glue("{round({euc_dist}, 2)}")
  ) |>
  ggplot(aes(x = walk_number, y = euc_dist, fill = dist_text)) +
  geom_col(alpha = .5) +
  geom_text(aes(label = dist_text), vjust = 1.5) +
  theme_classic() +
  labs(
    x = "Walk Number",
    y = "Euclidean Distance",
    fill = "Distance",
    title = "Euclidean Distance from Start to Finish",
    subtitle = "Grouped by Walk Number"
  ) +
  theme(legend.position = "none")
