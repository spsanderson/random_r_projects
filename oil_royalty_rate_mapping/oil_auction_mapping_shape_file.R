
# Load Libraries ----------------------------------------------------------

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(
    "rgdal",
    "leaflet",
    "dplyr",
    "magrittr",
    "readr",
    "knitr",
    "sp"
)


# USA Level Zipcode for 2015 ----------------------------------------------

tmp2 <- paste0(getwd(),"/oil_royalty_rate_mapping")
# url2 <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip"
# file <- basename(url2)
# download.file(url2, file)
# unzip(file, exdir = tmp2)
usa <- readOGR(dsn = tmp2, layer = "cb_2015_us_zcta510_500k", encoding = "UTF-8")
names(usa)[1] = "zipcode"

all_usa_zip <- read_csv("oil_royalty_rate_mapping/zip_code_database.csv")


# Get a specific state ----------------------------------------------------

specific_state <- all_usa_zip %>%
    filter(state == "LA") %>%
    select(zip, primary_city, county)

colnames(specific_state) <- c("zipcode", "City", "County")
specific_state$zipcode <- as.factor(specific_state$zipcode)
specific_state$County <- gsub("County", "", specific_state$County)


# Join shape file ---------------------------------------------------------

state_join <- full_join(usa@data, specific_state)
state_clean <- na.omit(state_join)

state_shape <- sp::merge(x = usa, y = state_clean, all.x = FALSE)

dim(state_shape)
