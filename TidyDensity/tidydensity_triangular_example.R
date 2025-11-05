library(TidyDensity)
library(tidyverse)

# Project task duration simulation using triangular distribution
tasks <- list(
  task1 = list(min = 5, mode = 8, max = 15),
  task2 = list(min = 3, mode = 5, max = 10),
  task3 = list(min = 7, mode = 10, max = 20),
  task4 = list(min = 2, mode = 4, max = 8)
)

# Simulate total project duration
n_sims <- 5000
total_durations <- numeric(n_sims)

for (i in 1:n_sims) {
  task_durations <- sapply(tasks, function(task) {
    tidy_triangular(.n = 2, .min = task$min, .max = task$max, .mode = task$mode)$y[1]
  })
  total_durations[i] <- sum(task_durations)
}

# Analyze results
duration_dist <- tidy_empirical(.x = total_durations, .num_sims = 1)

percentiles <- quantile(total_durations, c(0.1, 0.5, 0.9))

cat("Project Duration Analysis\n")
cat("10th percentile (optimistic):", round(percentiles[1], 1), "days\n")
cat("50th percentile (median):", round(percentiles[2], 1), "days\n")
cat("90th percentile (pessimistic):", round(percentiles[3], 1), "days\n")

# Visualize
tidy_autoplot(duration_dist, .plot_type = "density") +
  geom_vline(xintercept = percentiles[1], color = "green", linetype = "dashed") +
  geom_vline(xintercept = percentiles[2], color = "blue", linetype = "solid") +
  geom_vline(xintercept = percentiles[3], color = "red", linetype = "dashed") +
  labs(title = "Project Duration Distribution",
       subtitle = "Monte Carlo Simulation (5000 iterations)",
       x = "Total Duration (days)",
       y = "Density")
