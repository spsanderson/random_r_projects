library(RandomWalker)
library(tidyverse)

# Continuous Walks
ns <- 1L

set.seed(123)
rw_tbl <- bind_rows(
  brownian_motion(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Brownian Motion"),
  geometric_brownian_motion(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Geometric Brownian Motion"),
  random_beta_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Beta"),
  random_cauchy_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Cauchy"),
  random_chisquared_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Chisquared"),
  random_exponential_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Exponential"),
  random_f_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "F Distribution"),
  random_gamma_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Gamma"),
  random_logistic_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Logisitic"),
  random_lognormal_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Log-Normal"),
  random_normal_drift_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Random Normal Drift"),
  random_normal_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Random Normal"),
  random_t_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "T Distribution"),
  random_uniform_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Uniform"),
  random_weibull_walk(.num_walks = ns, .dimensions = 2) |>
    mutate(walk_type = "Weibull")
) |>
  select(step_number, x, y, walk_type)

rw_tbl |>
  ggplot(aes(x = x, y = y)) +
  facet_wrap(~ walk_type, scales = "free") +
  geom_path(aes(color = step_number)) +
  geom_point(
    data = rw_tbl |>
      group_by(walk_type) |>
      filter(
        step_number == max(step_number) |
          step_number == min(step_number)
      ),
      aes(color = step_number),
      size = 3
  ) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Random Walks for Continuous Distributions",
    subtitle = paste0(
      "Number of Simulations: ", ns, " - ",
      "Number of Steps: ", 100
    ),
    y = "Y Value",
    x = "X Value"
  ) +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank()
  )

# Discrete Walks
set.seed(123)
dw_tbl <- bind_rows(
    discrete_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Discrete"),
    random_binomial_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Binomial"),
    random_displacement_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Displacement"),
    random_geometric_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Geometric"),
    random_hypergeometric_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Hypergeometric"),
    random_multinomial_walk(.num_walks = ns, .dimensions = 2, .size = 10) |>
      mutate(walk_type = "Multinomial"),
    random_negbinomial_walk(.num_walks = ns, .dimensions = 2,
      .size = c(1, 1), .prob = c(.1, .1)) |>
      mutate(walk_type = "Negative Binomial"),
    random_poisson_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Poisson"),
    random_smirnov_walk(.num_walks = ns, .dimensions = 2, .z = seq(1, 2, by = .2),
  .sizes = c(2, 1)) |>
      mutate(walk_type = "Smirnov"),
    random_wilcoxon_sr_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Wilcoxon Signed Rank"),
    random_wilcox_walk(.num_walks = ns, .dimensions = 2) |>
      mutate(walk_type = "Wilcox")
  )

dw_tbl |>
  ggplot(aes(x = x, y = y)) +
  facet_wrap(~ walk_type, scales = "free") +
  geom_path(aes(color = step_number)) +
  geom_point(
    data = dw_tbl |>
      group_by(walk_type) |>
      filter(
        step_number == max(step_number) |
          step_number == min(step_number)
      ),
      aes(color = step_number),
      size = 3
  ) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Random Walks for Discrete Distributions",
    subtitle = paste0(
      "Number of Simulations: ", ns, " - ",
      "Number of Steps: ", 100
    ),
    y = "Y Value",
    x = "X Value"
  ) +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank()
  )