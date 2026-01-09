# Install if needed (uncomment)
# install.packages(c("lubridate", "openxlsx", "writexl"))

library(lubridate)
library(dplyr)
library(openxlsx) # For Excel export with optional manual merging

# Set seed for reproducibility
set.seed(123)

# Generate clean time series: 250 daily points starting from 2020-01-01
dates_clean <- seq(as.Date("2020-01-01"), by = "day", length.out = 250)
values <- cumsum(rnorm(250, mean = 0, sd = 1)) + 100 # Random walk time series

# Create data frame
df <- data.frame(Date = dates_clean, Value = round(values, 2))

# Introduce messiness: random inconsistent date formats
date_formats <- c(
  "%Y-%m-%d", # 2020-01-01
  "%d/%m/%Y", # 01/01/2020
  "%m/%d/%Y", # 01/01/2020 (US style)
  "%B %d, %Y", # January 01, 2020
  "%d-%b-%y", # 01-Jan-20
  "%Y%m%d" # 20200101
)

df$Date_Messy <- sapply(df$Date, function(d) {
  fmt <- sample(date_formats, 1)
  format(d, fmt)
})

# Add some additional messiness (typos, whitespace, missing, special chars)
set.seed(123)

prefix_choices <- c("", " ", "  ", "\t", "~", "*")
suffix_choices <- c("", " ", "  ", "%", "??")

n <- length(df$Value)
prefixes <- sample(
  prefix_choices,
  size = n,
  replace = TRUE,
  prob = c(0.7, 0.1, 0.1, 0.05, 0.05, 0.05)
)
suffixes <- sample(
  suffix_choices,
  size = n,
  replace = TRUE,
  prob = c(0.7, 0.1, 0.1, 0.05, 0.05)
)

# Randomly make some values missing or encoded strangely
missing_idx <- sample(1:250, 20)
df$Value_Messy <- paste0(
  prefixes,
  format(round(df$Value, 2), nsmall = 2),
  suffixes
)
df$Value_Messy

# Typos in some values (e.g., swapped digits)
typo_idx <- sample(setdiff(1:250, missing_idx), 15)
for (i in typo_idx) {
  val_char <- df$Value_Messy[i]
  if (!is.na(val_char) && val_char != "") {
    chars <- strsplit(val_char, "")[[1]]
    swap_pos <- sample(length(chars), 2)
    chars[c(swap_pos[1], swap_pos[2])] <- chars[c(swap_pos[2], swap_pos[1])]
    df$Value_Messy[i] <- paste(chars, collapse = "")
  }
}

# Final messy dataset (drop clean columns if desired)
messy_df <- df %>% select(Date_Messy, Value_Messy)

# View first rows
head(messy_df, 20)

# Export to csv
write.csv(messy_df, "messy_time_series.csv", row.names = FALSE)

# Export to Excel
write.xlsx(messy_df, "messy_time_series.xlsx")

# Optional: Export with manual merged cells (e.g., title or group headers)
wb <- createWorkbook()
addWorksheet(wb, "MessyData")
writeData(
  wb,
  "MessyData",
  "Time Series Data - Messy Version",
  startRow = 1,
  startCol = 1
)
writeData(
  wb,
  "MessyData",
  "Group 1 (First 100 points)",
  startRow = 3,
  startCol = 1
)
writeData(
  wb,
  "MessyData",
  messy_df,
  startRow = 5,
  startCol = 1,
  withFilter = TRUE
)

# Merge cells for headers
mergeCells(wb, "MessyData", cols = 1:2, rows = 1)
mergeCells(wb, "MessyData", cols = 1:2, rows = 3)

saveWorkbook(wb, "messy_time_series_with_merges.xlsx", overwrite = TRUE)
