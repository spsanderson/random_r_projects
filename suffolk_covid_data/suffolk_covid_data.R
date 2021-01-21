suppressPackageStartupMessages(library(dplyr))

url   <- "https://suffolkcountyny.gov/Departments/Health-Services/Health-Bulletins/Novel-Coronavirus"
page  <- xml2::read_html(url)
nodes <- page %>%
    rvest::html_nodes(".EDN_article_content") %>%
    rvest::html_nodes("a")

links <- nodes %>%
    rvest::html_attr("href") %>%
    as.list()

from_link_to_data <- function(links){
    
    links %>% 
        xml2::read_html() %>%
        rvest::html_nodes("li") %>%
        rvest::html_text(trim = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::slice(7,10:29)
    
}

link_data_tbl <- links %>%
    purrr::map_df(from_link_to_data) %>% 
    mutate(rpt_date = lubridate::dmy(value)) %>%
    mutate(rpt_date = zoo::na.locf(rpt_date)) %>%
    select(rpt_date, value)