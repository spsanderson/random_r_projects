library(RandomWalker)
library(dplyr)
library(ggplot2)
library(patchwork)

n <- 25
nw <- 12

x <- random_normal_walk(.num_walks = nw, .n = n, .samp = FALSE) |>
 select(cum_sum_y)
y <- random_normal_walk(.num_walks = nw, .n = n, .samp = FALSE) |>
 select(y)
xx <- predict(smooth.spline(x$cum_sum_y, spar = 0.005), seq(1, n, 0.01))$y[-1]
yy <- predict(smooth.spline(y$y, spar = 0.005), seq(1, n, 0.01))$y[-1]

df <- tibble(
 walk_number = rep(1:nw, each = length(xx)/nw) |> factor(levels = 1:nw),
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
  labs(caption = "Separate Random Walks") +
  theme_void()

p2 <- df |>
 ggplot(aes(color = walk_number)) +
 geom_path(
  aes(x = x, y = y, lwd = c(0, diff(y))),
  show.legend = FALSE
  ) +
 theme_void()

p3 <- df |>
  ggplot() +
  geom_path(aes(x = x, y = y, color = y, lwd = c(0, diff(y))), show.legend = FALSE) +
  scale_color_viridis_c(option = "viridis") +
  theme_void()

(p1 + (p2 / p3)) +
 plot_annotation(
 title = "Caligraphy in ggplot2 using RandomWalker",
 subtitle = paste0(nw, " Random Walks with ", n, " Steps"),
 caption = "All Walks Put Together"
 )
