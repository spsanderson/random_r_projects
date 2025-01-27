
# Function to get a valid user guess
get_user_guess <- function() {
  repeat {
    cat("Do you think the next roll will be\n")
    cat("(H)igher, (L)ower, or (S)ame?\n\n")
    cat("Enter H, L, or S to reflect your guess: ")
    ans <- toupper(readline())
    if (ans %in% c('H', 'L', 'S')) {
      return(ans)
    }
  }
}

# Roll two dice and return total
roll_dice <- function() {
  dice1 <- sample(1:6, 1)
  dice2 <- sample(1:6, 1)
  return(list(dice1 = dice1, dice2 = dice2, total = dice1 + dice2))
}

# Main game logic
main <- function() {
  # First roll
  roll1 <- roll_dice()
  cat(sprintf("First roll of the dice was %d and %d, for a total of %d.\n\n",
              roll1$dice1, roll1$dice2, roll1$total))
  
  # Get user's guess
  ans <- get_user_guess()
  
  # Second roll
  roll2 <- roll_dice()
  cat(sprintf("\nThe second roll was %d and %d, for a total of %d.\n\n",
              roll2$dice1, roll2$dice2, roll2$total))
  
  # Compare results
  if (ans == 'L') {
    if (roll2$total < roll1$total) {
      cat("Good job! You were right!\n")
      cat(sprintf("%d is lower than %d\n", roll2$total, roll1$total))
    } else {
      cat(sprintf("Sorry! %d is not lower than %d\n\n", roll2$total, roll1$total))
    }
  } else if (ans == 'H') {
    if (roll2$total > roll1$total) {
      cat("Good job! You were right!\n")
      cat(sprintf("%d is higher than %d\n", roll2$total, roll1$total))
    } else {
      cat(sprintf("Sorry! %d is not higher than %d\n\n", roll2$total, roll1$total))
    }
  } else if (ans == 'S') {
    if (roll2$total == roll1$total) {
      cat("Good job! You were right!\n")
      cat(sprintf("%d is the same as %d\n\n", roll2$total, roll1$total))
    } else {
      cat(sprintf("Sorry! %d is not the same as %d\n\n", roll2$total, roll1$total))
    }
  }
}

# Run the game
main()
