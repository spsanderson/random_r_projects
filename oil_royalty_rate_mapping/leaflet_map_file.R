
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

final_merged_tbl <- sp::merge(
    x = state_shape
    , y = final_tbl
    , all.x = FALSE
)

final_tbl %>% glimpse()


# Map leaflet -------------------------------------------------------------
# You will have to manually set these to the appropriate values for each 
# state, you can get them coordinates off of maps.google.com
# the smaller the sv_zoom the further out you are looking, so a higher number
# will zoom you in further to the ground level
sv_lat <- 31.2957899
sv_lon <- -92.5523868
sv_zoom <- 8

popup <- paste(
    "<strong>County: </strong>"
    , state_shape$County
    , "<br><strong>City: </strong>"
    , state_shape$City
    , "<br><strong>ZipCode: </strong>"
    , state_shape$zipcode
)

leaflet(state_shape) %>%
    setView(lng = sv_lon, lat = sv_lat, zoom = sv_zoom) %>%
    addTiles(group = "OSM (default)") %>%
    addPolygons(
        data = state_shape
        #, fillColor = ~pal(dsch_bin)
        #, fillOpacity = 0.7
        # , color = "#BDBDC3"
        #, weight = 0.7
        , popup = popup
    )
