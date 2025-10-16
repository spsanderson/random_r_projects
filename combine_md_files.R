# Libraries ----
library(tidyverse)

# Directory ----
## Make a list of input directories ----
base_path <- "C:/path/to/your/files/"
input_dirs <- list.dirs(base_path)[-1]
input_dir_tbl <- tibble(
  input_dir = input_dirs
) |>
  mutate(output_dir = paste0(input_dir, "/", basename(input_dir), "_combined_files.md"))

# If files exists that end in "_combined_files.md", delete them from disk ----
files_to_delete <- list.files(
  path = base_path,
  pattern = "_combined_files\\.md$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(files_to_delete) > 0) {
  cat("Deleting old combined files...\n")
  file.remove(files_to_delete)
}

# Write new combined files to disk ----
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
      cat("\n", "Searching for .md files in:", input_dir, "\n")
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
      cat("\n")
      cat("Writing combined content to:", output_dir, "\n")
      writeLines(combined_content, output_dir)

      cat("Successfully combined", length(md_files), ".md files into", output_dir, "\n")
      cat("Total lines written:", length(combined_content), "\n")
      cat("\n")
    }
  )

# Copy the combined files that have the pattern "_combined_files.md" to the following location ----
copy_to_dir <- "C:/path/to/your/files/Combined_Files"

# Create the destination directory if it doesn't exist
if (!dir.exists(copy_to_dir)) {
  dir.create(copy_to_dir, recursive = TRUE)
  cat("Created directory:", copy_to_dir, "\n")
}

# Copy all combined files
cat("\nCopying combined files to:", copy_to_dir, "\n")
files_to_copy <- input_dir_tbl$output_dir[file.exists(input_dir_tbl$output_dir)]
rlang::inform(message = glue::glue("Going to move the following file: {basename(files_to_copy)}"))

if (length(files_to_copy) > 0) {
  success_count <- 0
  for (file in files_to_copy) {
    dest_file <- file.path(copy_to_dir, basename(file))
    if (file.rename(from = file, to = dest_file)) {
      cat("Successfully moved:", basename(file), "\n")
      success_count <- success_count + 1
    } else {
      cat("Failed to move:", basename(file), "\n")
    }
  }
  cat("Total files moved:", success_count, "out of", length(files_to_copy), "\n")
} else {
  cat("No files found to move.\n")
}