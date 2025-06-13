# Write a program to find out how often a streak of six heads or a streak of six tails 
# comes up in a randomly generated list of heads and tails. Your program breaks up 
# the experiment into two parts: 
#   the first part generates a list of randomly selected 'heads' and 'tails' values, 
#   and the second part checks if there is a streak in it. 
# Put all of this code in a loop that repeats the experiment n times so we can 
# find out what percentage of the coin flips contains a streak of six heads or tails in a row. 
# As a hint, the function call sample(c('H','T'), size = 100, replace = TRUE) 
# will return a random sample list of H's and T's.
# The code should also print out the percentage of times a streak of six heads or tails occurs.
n <- 10000L  # Number of experiments
streak_length <- 6  # Length of the streak to check
streak_count <- 0  # Counter for streak occurrences
for (i in 1:n) {
  # Generate a random sample of heads and tails
  flips <- sample(c('H', 'T'), size = 100, replace = TRUE)
  
  # Check for streaks of six heads or tails
  for (j in 1:(length(flips) - streak_length + 1)) {
    if (all(flips[j:(j + streak_length - 1)] == 'H') || 
        all(flips[j:(j + streak_length - 1)] == 'T')) {
      streak_count <- streak_count + 1
      break  # Stop checking further once a streak is found
    }
  }
}
# Calculate the percentage of experiments with a streak
percentage_streaks <- (streak_count / n) * 100
# Print the result
cat(sprintf("Percentage of experiments with a streak of six heads or tails: %.2f%%\n", percentage_streaks))
