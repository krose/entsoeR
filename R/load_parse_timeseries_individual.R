

#' Helper function to parse a single time series node.
#'
#' The time series structure can be individual so I need to add the
#' possible nodes that can pop-up.
#'
#' @param ts_individual A single time series node.
load_parse_timeseries_individual <- function(ts_individual){

  ts_indi_list <- list()

  ts_indi_list$mrid <-
    ts_individual %>%
    rvest::html_node(xpath = "mrid") %>%
    rvest::html_text()

  ts_indi_list$businesstype <-
    ts_individual %>%
    rvest::html_node(xpath = "businesstype") %>%
    rvest::html_text()

  ts_indi_list$objectaggregation <-
    try(ts_individual %>%
          rvest::html_node(xpath = "objectaggregation") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$in_domain.mrid <-
    try(ts_individual %>%
          rvest::html_node(xpath = "in_domain.mrid") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$out_domain.mrid <-
    try(ts_individual %>%
          rvest::html_node(xpath = "out_domain.mrid") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$outbiddingzone_domain.mrid <-
    try(ts_individual %>%
          rvest::html_node(xpath = "outbiddingzone_domain.mrid") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$registeredresources.mrid <-
    try(ts_individual %>%
          rvest::html_node(xpath = "registeredresources.mrid") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$registeredresources.name <-
    try(ts_individual %>%
          rvest::html_node(xpath = "registeredresources.name") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$quantity_measure_unit.name <-
    try(ts_individual %>%
          rvest::html_node(xpath = "quantity_measure_unit.name") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$curvetype <-
    ts_individual %>%
  rvest::html_node(xpath = "curvetype") %>%
    rvest::html_text()

  ts_indi_list$cancelledts <-
    try(ts_individual %>%
          rvest::html_node(xpath = "cancelledts") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list <- lapply(ts_indi_list,
                         function(x){
                           if(stringr::str_detect(tolower(x), "error")){
                             return(as.character(NA))
                           } else {
                             return(x)
                           }
                         })

  ts_indi_df <- tibble::as_tibble(ts_indi_list)

  if(ts_indi_df$curvetype == "A01"){
    ts_indi_df$points <- list(load_parse_period(ts_individual))
  }

  ts_indi_df
}
