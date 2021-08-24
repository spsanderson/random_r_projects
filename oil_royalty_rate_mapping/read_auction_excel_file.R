
# Library Load ------------------------------------------------------------

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(
    "tidyverse",
    "readxl"
)

# Read Auction File -------------------------------------------------------
state <- "LA" # This is Lousiana, New Mexico is NM, Colorado is CO Texas is TX
sheet_name <- paste0(state,"_Dataset")
auction_file <- read_excel(
    path = "oil_royalty_rate_mapping/auction_structured.xlsx"
    , sheet = sheet_name
)
