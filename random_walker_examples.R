library(RandomWalker)
library(dplyr)
library(ggplot2)
library(tidyr)

# Get a random normal walk
set.seed(42)
rw_tbl <- random_wilcoxon_sr_walk(.num_walks = 12, .dimensions = 2, .n = 100)
ci_tbl <- rw_tbl |>
  # Group by walk_number
  group_by(walk_number) |>
  # Calculate the CI for each group
  summarise(
    ci_x = confidence_interval(x),
    ci_y = confidence_interval(y)
  ) |>
  # Unnest the ci_values column
  unnest_wider(ci_x) |>
  rename(
    lower_x = lower,
    upper_x = upper
  ) |>
  unnest_wider(ci_y) |>
  rename(
    lower_y = lower,
    upper_y = upper
  ) |>
  ungroup() |>
  # Get the minimum lower and maxiumum upper
  summarise(
    lower_x = min(lower_x),
    lower_y = min(lower_y),
    upper_x = max(upper_x),
    upper_y = max(upper_y)
  )

# Which walk numbers are outside of the ci_tbl range
outliers <- rw_tbl |>
  filter(
    (y < ci_tbl$lower_y | y > ci_tbl$upper_y) |
      (x < ci_tbl$lower_x | x > ci_tbl$upper_x)
    )|>
  pull(walk_number) |>
  unique()

rw_tbl |>
  ggplot(aes(x = x, y = y, group = walk_number)) +
  geom_hline(data = ci_tbl, aes(yintercept = lower_x), linetype = "dashed", color = "black") +
  geom_hline(data = ci_tbl, aes(yintercept = upper_x), linetype = "dashed", color = "black") +
  geom_vline(data = ci_tbl, aes(xintercept = lower_y), linetype = "dashed", color = "black") +
  geom_vline(data = ci_tbl, aes(xintercept = upper_y), linetype = "dashed", color = "black") +
  geom_path(
    data = rw_tbl |> 
      filter(!walk_number %in% outliers),
    aes(x = x, y = y, group = walk_number, color = walk_number),
    linewidth = 1
  ) +
  # Make the lines of the outliers red and bold
  geom_path(
    data = rw_tbl |> 
      filter(walk_number %in% outliers), 
    aes(x = x, y = y, group = walk_number), 
    color = "red",
    linetype = "dashed",
    alpha = 0.5
  ) +
  theme_minimal() +
  labs(
    title = "2D Random Walks with Outliers Highlighted",
    x = "X",
    y = "Y"
  ) +
  theme(legend.position = "none")

# Just the non-outliers
rw_tbl |>
  # Get only the non-outlier walks
  filter(!walk_number %in% outliers) |>
  ggplot() +
  geom_path(
    aes(x =  x, y =  y, group = walk_number, color = walk_number),
    alpha = 0.618
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2D Random Walk Non-Outliers Only",
    x = "X",
    y = "Y"
  )

# Just viewing the outliers
rw_tbl |>
  # Get only the outlier walks
  filter(walk_number %in% outliers) |>
  ggplot() +
  geom_path(
    aes(x =  x, y =  y, group = walk_number, color = walk_number),
    alpha = 0.618
  ) +
  geom_hline(data = ci_tbl, aes(yintercept = lower_x), linetype = "dashed", color = "black") +
  geom_hline(data = ci_tbl, aes(yintercept = upper_x), linetype = "dashed", color = "black") +
  geom_vline(data = ci_tbl, aes(xintercept = lower_y), linetype = "dashed", color = "black") +
  geom_vline(data = ci_tbl, aes(xintercept = upper_y), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2D Random Walk Outliers Only",
    x = "X",
    y = "Y"
  )

rw_tbl |>
  ggplot(aes(x =  x, y =  y)) +
  facet_wrap(~ walk_number, scales = "free") +
  geom_path(aes(color = step_number), linewidth = 0.5) +
  geom_point(data = rw_tbl |> 
               group_by(walk_number) |> 
               filter(step_number == min(step_number) | step_number == max(step_number)) |> 
               ungroup(), 
             aes(color = step_number), size = 3) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2D Random Wilcox Walks with Walk Number Facets and Steps Highlighted",
    x = "X",
    y = "Y"
  )

