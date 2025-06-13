# Conway's Game of Life in R

WIDTH <- 8
HEIGHT <- 8

# Initialize the grid with random values
nextCells <- vector("list", WIDTH)
for (x in seq_len(WIDTH)) {
  column <- character(HEIGHT)
  for (y in seq_len(HEIGHT)) {
    if (sample(0:1, 1) == 0) {
      column[y] <- "#"  # Alive cell
    } else {
      column[y] <- " "  # Dead cell
    }
  }
  nextCells[[x]] <- column
}

while (TRUE) {
  cat(rep("\n", 5), sep = "")  # Clear screen with newlines
  currentCells <- lapply(nextCells, identity)  # Deep copy
  
  # Print the current cells
  for (y in seq_len(HEIGHT)) {
    for (x in seq_len(WIDTH)) {
      cat(currentCells[[x]][y])
    }
    cat("\n")
  }
  
  # Calculate the next generation
  for (x in seq_len(WIDTH)) {
    for (y in seq_len(HEIGHT)) {
      leftCoord  <- ((x - 2) %% WIDTH) + 1
      rightCoord <- (x %% WIDTH) + 1
      aboveCoord <- ((y - 2) %% HEIGHT) + 1
      belowCoord <- (y %% HEIGHT) + 1
      
      # Count live neighbors
      numNeighbors <- 0
      if (currentCells[[leftCoord]][aboveCoord] == "#") numNeighbors <- numNeighbors + 1
      if (currentCells[[x]][aboveCoord] == "#")        numNeighbors <- numNeighbors + 1
      if (currentCells[[rightCoord]][aboveCoord] == "#") numNeighbors <- numNeighbors + 1
      if (currentCells[[leftCoord]][y] == "#")         numNeighbors <- numNeighbors + 1
      if (currentCells[[rightCoord]][y] == "#")        numNeighbors <- numNeighbors + 1
      if (currentCells[[leftCoord]][belowCoord] == "#") numNeighbors <- numNeighbors + 1
      if (currentCells[[x]][belowCoord] == "#")        numNeighbors <- numNeighbors + 1
      if (currentCells[[rightCoord]][belowCoord] == "#") numNeighbors <- numNeighbors + 1
      
      # Apply Game of Life rules
      if (currentCells[[x]][y] == "#" && (numNeighbors == 2 || numNeighbors == 3)) {
        nextCells[[x]][y] <- "#"
      } else if (currentCells[[x]][y] == " " && numNeighbors == 3) {
        nextCells[[x]][y] <- "#"
      } else {
        nextCells[[x]][y] <- " "
      }
    }
  }
  
  Sys.sleep(1)  # Pause to reduce flickering
}
