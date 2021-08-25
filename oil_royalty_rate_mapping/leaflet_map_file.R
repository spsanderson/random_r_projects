
# Library load ------------------------------------------------------------

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(
    "tmaptools",
    "tidyverse",
    "leaflet",
    "readxl",
    "rgdal",
    "sp",
    "janitor"
)


# Read in files -----------------------------------------------------------

geocoded_tbl <- read_rds("oil_royalty_rate_mapping/geocoded_tibbles/la_dataset.rds")

state <- "LA" # This is Lousiana, New Mexico is NM, Colorado is CO Texas is TX
sheet_name <- paste0(state,"_Dataset")
auction_file <- read_excel(
    path = "oil_royalty_rate_mapping/auction_structured.xlsx"
    , sheet = sheet_name
) %>%
    clean_names()

# Now lets make a partial address that can be geocoded
final_tbl <- auction_file %>%
    mutate(partial_address = paste0(county, ", ", state)) %>%
    left_join(geocoded_tbl)

final_tbl %>% glimpse()
