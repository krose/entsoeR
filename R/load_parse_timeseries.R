

load_parse_timeseries <- function(e_content, doc_name){

  e_ts <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "timeseries") %>%
    purrr::map(~load_parse_timeseries_individual(.)) %>%
    dplyr::bind_rows()

  e_ts
}
