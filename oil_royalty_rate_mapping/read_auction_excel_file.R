
# Library Load ------------------------------------------------------------

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(
    "tidyverse",
    "readxl",
    "writexl",
    "janitor"
)

# Read Auction File -------------------------------------------------------
# NOTE ---- 
# You must change the name of each worksheet in the file to something like:
# LA_Dataset for Lousiana, NM_Dataset for New Mexico

state <- "TX" # This is Lousiana, New Mexico is NM, Colorado is CO Texas is TX
sheet_name <- paste0(state,"_Dataset")
auction_file <- read_excel(
    path = "oil_royalty_rate_mapping/auction_structured.xlsx"
    , sheet = sheet_name
) %>%
    clean_names()

# Now lets make a partial address that can be geocoded
auction_file %>%
    mutate(partial_address = paste0(county, ", ", state)) %>%
    # Write the new file out to use in geocoding
    write_xlsx(path = "oil_royalty_rate_mapping/new_auction_file.xlsx")

