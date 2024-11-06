# Load required library
if (!require(httr)) {
  install.packages("httr")
}
library(httr)

# Define directory name
pdf_dir <- "downloaded_pdfs"

# Check if directory exists
if (dir.exists(pdf_dir)) {
  cat(sprintf("Directory '%s' already exists.\n", pdf_dir))
  # Ask user if they want to continue
  response <- readline(prompt = "Do you want to continue downloading to this directory? (y/n): ")
  if (tolower(response) != "y") {
    stop("Download cancelled by user.")
  }
} else {
  cat(sprintf("Creating new directory: '%s'\n", pdf_dir))
  dir.create(pdf_dir)
}

# Change working directory to pdf_dir
setwd(pdf_dir)

# Base URL
base_url <- "https://static.project2025.org/2025_MandateForLeadership_CHAPTER-"
afterword_url <- "https://static.project2025.org/2025_MandateForLeadership_AFTERWORD.pdf"

# Function to download PDF
download_pdf <- function(chapter_number) {
  # Format chapter number with leading zero if less than 10
  formatted_chapter <- sprintf("%02d", chapter_number)
  
  # Construct full URL
  url <- paste0(base_url, formatted_chapter, ".pdf")
  
  # Construct output filename
  output_file <- paste0("chapter_", formatted_chapter, ".pdf")
  
  # Check if file already exists
  if (file.exists(output_file)) {
    cat(sprintf("Chapter %s already exists, skipping...\n", formatted_chapter))
    return()
  }
  
  # Try to download the file
  tryCatch({
    response <- GET(url, write_disk(output_file, overwrite = TRUE))
    
    # Check if download was successful
    if (status_code(response) == 200) {
      cat(sprintf("Successfully downloaded Chapter %s\n", formatted_chapter))
    } else {
      cat(sprintf("Failed to download Chapter %s (Status code: %d)\n", 
                  formatted_chapter, status_code(response)))
    }
  }, error = function(e) {
    cat(sprintf("Error downloading Chapter %s: %s\n", formatted_chapter, e$message))
  })
}

# Function to download afterword
download_afterword <- function() {
  output_file <- "afterword.pdf"
  
  # Check if file already exists
  if (file.exists(output_file)) {
    cat("Afterword already exists, skipping...\n")
    return()
  }
  
  # Try to download the file
  tryCatch({
    response <- GET(afterword_url, write_disk(output_file, overwrite = TRUE))
    
    # Check if download was successful
    if (status_code(response) == 200) {
      cat("Successfully downloaded Afterword\n")
    } else {
      cat(sprintf("Failed to download Afterword (Status code: %d)\n", 
                  status_code(response)))
    }
  }, error = function(e) {
    cat(sprintf("Error downloading Afterword: %s\n", e$message))
  })
}

# Download PDFs for chapters 1 to 30
cat("\n#### Downloading Chapters ####\n")
for (i in 1:30) {
  download_pdf(i)
  # Add a small delay to avoid overwhelming the server
  Sys.sleep(1)
}

# Download afterword
cat("\n#### Downloading Afterword ####\n")
download_afterword()

cat("\n#### Download process completed ####\n")
