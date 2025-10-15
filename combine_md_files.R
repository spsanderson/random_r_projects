# Libraries ----
library(tidyverse)

# Directory ----
## Make a list of input directories ----
base_path <- "C:/file/path/"
input_dirs <- list.dirs(base_path)[-1]
input_dir_tbl <- tibble(
  input_dir = input_dirs
) |>
  mutate(output_dir = paste0(input_dir, "/", basename(input_dir), "_combined_files.md"))

input_dir_tbl |>
  group_split(input_dir) |>
  imap(
    .f = function(obj, id){
      input_dir = obj$input_dir
      output_dir = obj$output_dir

      # Check if the directory exists
      if (!dir.exists(input_dir)) {
        stop(paste("Error: Directory", input_dir, "does not exist."))
      }

      # List all .md files in the directory ----
      cat("Searching for .md files in:", input_dir, "\n")
      md_files <- list.files(path = input_dir, pattern = "\\.md$", full.names = TRUE)

      # Check if any .md files were found
      if (length(md_files) == 0) {
        stop("No .md files found in the specified directory.")
      }

      cat("Found", length(md_files), ".md files:\n")
      for (file in md_files) {
        cat("-", basename(file), "\n")
      }

      # Read and combine the contents of all .md files
      cat("\nReading and combining files...\n")
      combined_content <- character(0)

      for (file in md_files) {
        cat("Processing:", basename(file), "\n")
        
        # Add a header separator for each file (optional)
        file_header <- paste("\n<!-- Content from:", basename(file), "-->\n")
        combined_content <- c(combined_content, file_header)
        
        # Read the file content
        file_content <- readLines(file, warn = FALSE)
        combined_content <- c(combined_content, file_content)
        
        # Add some spacing between files
        combined_content <- c(combined_content, "\n")
      }

      # Write the combined content to the output file
      cat("Writing combined content to:", output_dir, "\n")
      writeLines(combined_content, output_dir)

      cat("Successfully combined", length(md_files), ".md files into", output_dir, "\n")
      cat("Total lines written:", length(combined_content), "\n")
    }
  )

