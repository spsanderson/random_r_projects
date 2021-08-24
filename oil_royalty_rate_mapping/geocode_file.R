
# Library Load ------------------------------------------------------------

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(
    "tidyverse",
    "readxl",
    "tmaptools",
    "leaflet"
)


# Read in new auction file ------------------------------------------------

auction_file_tbl <- read_excel(
    path = "oil_royalty_rate_mapping/new_auction_file.xlsx"
)

# Make a distinct list of places to geocode so we don't geocode the same place
# more than once
address_list <- auction_file_tbl %>%
    select(partial_address) %>%
    distinct() %>%
    pull()

# Map the geocode_OSM function to the vector
geocode_tbl <- address_list %>%
    map(function(x) geocode_OSM(x, return.first.only = TRUE, as.data.frame = TRUE)) %>%
    map_dfr(~ as.data.frame(.))

# Coerce to a tibble and rename columns
geocode_tbl <- geocode_tbl %>%
    as_tibble() %>%
    select(query, lat, lon) %>%
    set_names("partial_address","lattitude","longitude")

