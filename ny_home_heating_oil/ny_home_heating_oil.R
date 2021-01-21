suppressPackageStartupMessages(library(dplyr))

url  <- "https://www.eia.gov/opendata/qb.php?sdid=PET.W_EPD2F_PRS_SNY_DPG.W"
page <- xml2::read_html(url)
node <- rvest::html_node(
    x = page
    , xpath = "/html/body/div[1]/div/section/div/div/div[3]/div[1]/table"
)
ny_tbl <- node %>%
    rvest::html_table() %>%
    tibble::as_tibble() %>%
    purrr::set_names('series_name','period','frequency','value','units') %>%
    dplyr::select(period, frequency, value, units, series_name) %>%
    dplyr::mutate(period = lubridate::ymd(period)) %>%
    dplyr::arrange(period)

ny_tbl %>%
    timetk::plot_time_series(.date_var = period, .value = value)

ny_tbl %>%
    timetk::plot_time_series_cv_plan(.date_var = period, .value = value)