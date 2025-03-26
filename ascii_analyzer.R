ascii_analyzer_file <- function(file_path) {
  # Input validation
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Read the file content
  text_content <- tryCatch({
    readLines(file_path, warn = FALSE)
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })
  
  # Combine all lines into a single string
  full_text <- paste(text_content, collapse = "\n")
  
  # Create ASCII mapping for first 128 characters
  ascii_dict <- c(
    "Null character", "Start of Heading", "Start of Text", "End of Text",
    "End of Transmission", "Enquiry", "Acknowledge", "Bell, Alert",
    "Backspace", "Horizontal Tab", "Line Feed", "Vertical Tab",
    "Form Feed", "Carriage Return", "Shift Out", "Shift In","Data Link Escape",
    "Device Control One (XON)", "Device Control Two", "Device Control Three (XOFF)",
    "Device Control Four", "Negative Acknowledge", "Synchronous Idle", "End of Transmission Block",
    "Cancel", "End of Medium", "Substitute", "Escape", "File Separator",
    "Group Separator", "Record Separator", "Unit Separator", "Space",
    "Exclamation mark", "Double quotes", "Number sign", "Dollar sign",
    "Percent sign", "Ampersand", "Single quote", "Left parenthesis",
    "Right parenthesis", "Asterisk", "Plus", "Comma",
    "Hyphen, Minus", "Period, Dot", "Solidus, Slash", "Digit Zero",
    "Digit One", "Digit Two", "Digit Three", "Digit Four",
    "Digit Five", "Digit Six", "Digit Seven", "Digit Eight",
    "Digit Nine", "Colon", "Semicolon", "Less than",
    "Equal", "Greater than", "Question mark", "At symbol",
    "Uppercase A", "Uppercase B", "Uppercase C", "Uppercase D",
    "Uppercase E", "Uppercase F", "Uppercase G", "Uppercase H",
    "Uppercase I", "Uppercase J", "Uppercase K", "Uppercase L",
    "Uppercase M", "Uppercase N", "Uppercase O", "Uppercase P",
    "Uppercase Q", "Uppercase R", "Uppercase S", "Uppercase T",
    "Uppercase U", "Uppercase V", "Uppercase W", "Uppercase X",
    "Uppercase Y", "Uppercase Z", "Left square bracket", "Backslash",
    "Right square bracket", "Caret", "Underscore",
    "Grave accent", "Lowercase a", "Lowercase b",
    "Lowercase c", "Lowercase d", "Lowercase e",
    "Lowercase f", "Lowercase g", "Lowercase h",
    "Lowercase i", "Lowercase j", "Lowercase k",
    "Lowercase l", "Lowercase m", "Lowercase n",
    "Lowercase o", "Lowercase p", "Lowercase q",
    "Lowercase r", "Lowercase s", "Lowercase t",
    "Lowercase u", "Lowercase v", "Lowercase w",
    "Lowercase x", "Lowercase y", "Lowercase z",
    "Left curly brace", "Vertical bar", "Right curly brace", "Tilde",
    "Delete"
    # Add more mappings as needed
  )
  
  # Convert text to ASCII values
  char_vector <- unlist(strsplit(full_text, NULL))
  ascii_values <- as.integer(charToRaw(paste(char_vector, collapse="")))
  
  # Count frequencies
  freq_table <- table(factor(ascii_values, levels = 0:127))
  
  # Create and format output
  cat("Ascii Value       Ascii Symbol                                                                                     Frequency\n")
  cat("-----------       ------------                                    ---------\n")
  
  for(i in 1:length(ascii_dict)) {
    cat(sprintf("%03d               %-50s %10d\n",
                i-1,
                ascii_dict[i],
                freq_table[i]))
  }
  
  # Return invisible data frame for potential further processing
  invisible(data.frame(
    AsciiValue = 0:(length(ascii_dict)-1),
    AsciiSymbol = ascii_dict,
    Frequency = as.vector(freq_table[1:length(ascii_dict)])
  ))
}
