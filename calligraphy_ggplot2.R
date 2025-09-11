library(RandomWalker)
library(dplyr)
library(ggplot2)
library(patchwork)

n <- 250
nw <- 3

x <- random_weibull_walk(.num_walks = nw, .n = n, .samp = FALSE) |>
 select(cum_sum_y)
y <- random_uniform_walk(.num_walks = nw, .n = n, .samp = FALSE) |>
 select(y)
xx <- predict(smooth.spline(x$cum_sum_y, spar = 0.005), seq(1, n, 0.01))$y[-1]
yy <- predict(smooth.spline(y$y, spar = 0.005), seq(1, n, 0.01))$y[-1]

x_fns <- get_attributes(x)$fns |> convert_snake_to_title_case()
y_fns <- get_attributes(y)$fns |> convert_snake_to_title_case()

df <- tibble(
 walk_number = paste0("Sim ", rep(1:nw, each = length(xx)/nw)) |> 
   factor(levels = paste0("Sim ", 1:nw)),
 x = xx,
 y = yy
 )

p1 <- df |>
  ggplot(aes(color = walk_number)) +
  facet_wrap(~ walk_number, scales = "free") +
  geom_path(
    aes(x = x, y = y, lwd = c(0, diff(y))),
    show.legend = FALSE
  ) +
  scale_color_viridis_d(option = "viridis") +
  labs(caption = "Separate Random Walks") +
  theme_void()

p2 <- df |>
 ggplot(aes(color = walk_number)) +
 geom_path(
  aes(x = x, y = y, lwd = c(0, diff(y))),
  show.legend = FALSE
  ) +
  scale_color_viridis_d(option = "viridis") +
  labs(caption = "All Random Walks Put Together by Factors") +
  theme_void()

p3 <- df |>
  ggplot() +
  geom_path(aes(x = x, y = y, color = y, lwd = c(0, diff(y))), show.legend = FALSE) +
  scale_color_viridis_c(option = "plasma") +
  theme_void()

(p1 + (p2 / p3)) +
 plot_annotation(
 title = "Caligraphy in ggplot2 using RandomWalker",
 subtitle = paste0(nw, " Random Walks with ", n, " Steps", "\n",
                  "Using: ", x_fns, ", ", y_fns),
 caption = "All Walks Put Together Scale Color Plasma"
 )
