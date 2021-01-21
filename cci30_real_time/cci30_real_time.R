suppressPackageStartupMessages(library(dplyr))

cci30 <- xml2::read_html("https://cci30.com/")

tbl1 <- cci30 %>% 
    rvest::html_node(xpath = "/html/body/div[2]/div/div/div/div[2]/div[1]/table") %>% 
    rvest::html_table(header = 1) %>%
    tibble::as_tibble() %>%
    dplyr::select(2:5) %>%
    purrr::set_names(
        "name","price","market_cap","daily_change"
    )

tbl2 <- cci30 %>% 
    rvest::html_node(xpath = "/html/body/div[2]/div/div/div/div[2]/div[2]/table") %>% 
    rvest::html_table(header = 1) %>%
    tibble::as_tibble() %>%
    dplyr::select(2:5) %>%
    purrr::set_names(
        "name","price","market_cap","daily_change"
    )

tbl <- tbl1 %>%
    dplyr::union(tbl2)

tbl