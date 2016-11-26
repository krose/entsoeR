

load_parse_timeseries <- function(e_content){

  e_ts <-
    e_content %>% rvest::xml_node("gl_marketdocument") %>%
    rvest::xml_nodes(xpath = "timeseries") %>%
    purrr::map(~load_parse_timeseries_individual(.)) %>%
    dplyr::bind_rows()

  e_ts
}
