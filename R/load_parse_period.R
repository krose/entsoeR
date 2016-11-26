

load_parse_period <- function(ts_individual){

  period_start <-
    ts_individual %>% .[["period"]] %>%
    rvest::xml_node(xpath = "timeinterval") %>%
    rvest::xml_node("start") %>%
    rvest::xml_text()

  resolution <-
    ts_individual %>% .[["period"]] %>%
    rvest::xml_node(xpath = "resolution") %>%
    rvest::xml_text()

  resolution_qty <- stringr::str_extract(resolution, "[0-9]{1,4}") %>% as.integer()
  resolution_hm <- stringr::str_sub(string = resolution, start = -1L)

  datetime <-
    ts_individual %>%
    .[["period"]] %>%
    rvest::xml_nodes(xpath = "point") %>%
    rvest::xml_nodes(xpath = "position") %>%
    rvest::xml_text() %>%
    as.numeric()

  if(resolution_hm == "M"){
    datetime <-
      lubridate::ymd_hm(period_start, tz = "UTC") +
      lubridate::minutes(datetime * resolution_qty - 1 * resolution_qty)
  } else if(resolution_hm == "H"){
    datetime <-
      lubridate::ymd_hm(period_start, tz = "UTC") +
      lubridate::hours(datetime * resolution_qty - 1 * resolution_qty)
  } else {
    stop("The date resolution is not supported")
  }


  quantity <-
    ts_individual %>%
    .[["period"]] %>%
    rvest::xml_nodes(xpath = "point") %>%
    rvest::xml_nodes(xpath = "quantity") %>%
    rvest::xml_text() %>%
    as.numeric()

  point_df <- tibble(datetime = datetime, quantity = quantity)

  secondary_quantity <-
    ts_individual %>%
    .[["period"]] %>%
    rvest::xml_nodes(xpath = "point") %>%
    rvest::xml_nodes(xpath = "secondary_quantity") %>%
    rvest::xml_text() %>%
    as.numeric()
  if(length(secondary_quantity) > 0){
    point_df$secondaryquantity <- secondary_quantity
  }

  point_df
}
