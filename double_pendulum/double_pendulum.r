# =============================================================================
#  Double Pendulum Simulator in R
#  Equations of Motion: Lagrangian formulation
#  Numerical Integration: deSolve::ode() with RK4 method
#  Visualization: ggplot2 + gganimate (animated) + static trajectory plot
#
#  Required packages (install once):
#    install.packages(c("deSolve", "ggplot2", "gganimate", "gifski", "dplyr"))
#
#  Usage: Source this file or run section by section.
# =============================================================================

library(deSolve)
library(ggplot2)
library(gganimate)
library(dplyr)

# ── 1. Configurable Parameters ────────────────────────────────────────────────
params <- c(
  m1 = 1, # mass of bob 1 (kg)
  m2 = 1, # mass of bob 2 (kg)
  l1 = 1, # length of rod 1 (m)
  l2 = 1, # length of rod 2 (m)
  g = 9.81 # gravitational acceleration (m/s^2)
)

# Initial state: [theta1, theta2, omega1, omega2]
# Angles measured from vertical (radians); omegas are angular velocities
state0 <- c(
  theta1 = pi / 2, # initial angle of rod 1 (90 degrees from vertical)
  theta2 = pi / 2, # initial angle of rod 2 (90 degrees from vertical)
  omega1 = 0, # initial angular velocity of rod 1
  omega2 = 0 # initial angular velocity of rod 2
)

# Time vector: 0 to 20 seconds, step = 0.05 s
times <- seq(0, 20, by = 0.05)

# ── 2. Equations of Motion (Lagrangian ODEs) ──────────────────────────────────
# State vector: y = c(theta1, theta2, omega1, omega2)
# delta = theta2 - theta1
# denom = l * (2*m1 + m2 - m2 * cos(2*delta))
#
# d(theta1)/dt = omega1
# d(theta2)/dt = omega2
# d(omega1)/dt = [ -g*(2m1+m2)*sin(theta1) - m2*g*sin(theta1-2*theta2)
#                  - 2*sin(delta)*m2*(omega2^2*l2 + omega1^2*l1*cos(delta)) ]
#                / (l1 * (2*m1 + m2 - m2*cos(2*delta)))
# d(omega2)/dt = [ 2*sin(delta)*(omega1^2*l1*(m1+m2) + g*(m1+m2)*cos(theta1)
#                  + omega2^2*l2*m2*cos(delta)) ]
#                / (l2 * (2*m1 + m2 - m2*cos(2*delta)))

double_pendulum <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    delta <- theta2 - theta1
    denom <- 2 * m1 + m2 - m2 * cos(2 * delta) # shared denominator

    dtheta1 <- omega1
    dtheta2 <- omega2

    domega1 <- (-g *
      (2 * m1 + m2) *
      sin(theta1) -
      m2 * g * sin(theta1 - 2 * theta2) -
      2 * sin(delta) * m2 * (omega2^2 * l2 + omega1^2 * l1 * cos(delta))) /
      (l1 * denom)

    domega2 <- (2 *
      sin(delta) *
      (omega1^2 *
        l1 *
        (m1 + m2) +
        g * (m1 + m2) * cos(theta1) +
        omega2^2 * l2 * m2 * cos(delta))) /
      (l2 * denom)

    list(c(dtheta1, dtheta2, domega1, domega2))
  })
}

# ── 3. Solve the ODEs ─────────────────────────────────────────────────────────
cat("Solving double pendulum ODEs...\n")
sol <- ode(
  y = state0,
  times = times,
  func = double_pendulum,
  parms = params,
  method = "rk4" # explicit 4th-order Runge-Kutta
)
cat(sprintf("Done. %d time steps solved.\n", nrow(sol)))

# ── 4. Convert Angles to Cartesian Coordinates ────────────────────────────────
df <- as.data.frame(sol) %>%
  mutate(
    # Bob 1 position (pivot at origin)
    x1 = params["l1"] * sin(theta1),
    y1 = -params["l1"] * cos(theta1),
    # Bob 2 position (relative to bob 1)
    x2 = x1 + params["l2"] * sin(theta2),
    y2 = y1 - params["l2"] * cos(theta2),
    # Frame index for animation
    frame = row_number()
  )

cat(sprintf("Coordinate ranges:\n"))
cat(sprintf("  x2: [%.3f, %.3f]\n", min(df$x2), max(df$x2)))
cat(sprintf("  y2: [%.3f, %.3f]\n", min(df$y2), max(df$y2)))

# ── 5. Static Trajectory Plot ─────────────────────────────────────────────────
p_traj <- ggplot(df, aes(x = x2, y = y2, color = time)) +
  geom_path(linewidth = 0.4, alpha = 0.85) +
  scale_color_viridis_c(option = "plasma", name = "Time (s)") +
  coord_equal() +
  labs(
    title = "Double Pendulum — Trajectory of Bob 2",
    #subtitle = expression(theta[1] == theta[2] == pi/2 ~ ", " ~ m[1] == m[2] == 1 ~ "kg, " ~ L[1] == L[2] == 1 ~ "m"),
    x = "x (m)",
    y = "y (m)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(
  "double_pendulum_trajectory.png",
  p_traj,
  width = 6,
  height = 6,
  dpi = 150,
  bg = "white"
)
cat("Saved: double_pendulum_trajectory.png\n")

# ── 6. Animated Pendulum Visualization ───────────────────────────────────────
# Build a tidy data frame for each frame containing:
#   - The two rod segments (origin→bob1, bob1→bob2)
#   - The trail of bob 2 (last 30 steps)

trail_len <- 30 # number of frames in the trailing path

# Rod segments (two rows per frame: rod1 and rod2)
rods <- df %>%
  select(time, frame, x1, y1, x2, y2) %>%
  mutate(
    # Rod 1: pivot (0,0) → bob1
    r1_x = x1,
    r1_y = y1,
    # Rod 2: bob1 → bob2
    r2_x = x2,
    r2_y = y2
  )

# Build trail: for each frame, include the last `trail_len` positions of bob 2
trail_df <- lapply(df$frame, function(i) {
  start_i <- max(1, i - trail_len)
  slice(df, start_i:i) %>%
    mutate(anim_frame = i, alpha_val = seq(0.1, 1, length.out = n()))
}) %>%
  bind_rows()

# Main animation: pendulum rods + bobs + trail
p_anim <- ggplot() +
  # Trail of bob 2
  geom_path(
    data = trail_df,
    aes(x = x2, y = y2, group = anim_frame, alpha = alpha_val),
    color = "#e84393",
    linewidth = 0.8
  ) +
  # Rod 1: pivot to bob 1
  geom_segment(
    data = rods,
    aes(x = 0, y = 0, xend = x1, yend = y1),
    color = "grey80",
    linewidth = 1.5
  ) +
  # Rod 2: bob 1 to bob 2
  geom_segment(
    data = rods,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = "grey80",
    linewidth = 1.5
  ) +
  # Bob 1 (gold)
  geom_point(
    data = rods,
    aes(x = x1, y = y1),
    color = "#f5c542",
    size = 5
  ) +
  # Bob 2 (pink)
  geom_point(
    data = rods,
    aes(x = x2, y = y2),
    color = "#e84393",
    size = 5
  ) +
  # Pivot point
  geom_point(aes(x = 0, y = 0), color = "white", size = 3) +
  # Time label
  geom_text(
    data = rods,
    aes(x = -2.0, y = 2.0, label = sprintf("t = %.1f s", time)),
    color = "grey90",
    size = 4,
    hjust = 0
  ) +
  coord_equal(xlim = c(-2.2, 2.2), ylim = c(-2.2, 2.2)) +
  scale_alpha_identity() +
  labs(
    title = "Double Pendulum Simulation",
    #subtitle = expression(m[1] == m[2] == 1 ~ "kg," ~ L[1] == L[2] == 1 ~ "m," ~ theta[1] == theta[2] == 90*degree),
    x = "x (m)",
    y = "y (m)"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#0d0d0d", color = NA),
    panel.background = element_rect(fill = "#0d0d0d", color = NA),
    plot.title = element_text(
      color = "white",
      face = "bold",
      hjust = 0.5,
      size = 14
    ),
    plot.subtitle = element_text(color = "grey70", hjust = 0.5, size = 10),
    axis.title = element_text(color = "grey70"),
    axis.text = element_text(color = "grey60")
  ) +
  # Animate: one frame per time step
  transition_manual(frame)

cat("Rendering animation (this may take 1–3 minutes)...\n")
anim <- animate(
  p_anim,
  nframes = nrow(df),
  fps = 20,
  width = 500,
  height = 500,
  renderer = gifski_renderer("double_pendulum.gif")
)
cat("Saved: double_pendulum.gif\n")

cat("\n=== Simulation complete! ===\n")
cat("Output files:\n")
cat("  double_pendulum_trajectory.png  — static trajectory of bob 2\n")
cat("  double_pendulum.gif             — animated pendulum\n")
