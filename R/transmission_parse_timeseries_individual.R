

#' Helper function to parse a single time series node.
#'
#' The time series structure can be individual so I need to add the
#' possible nodes that can pop-up.
#'
#' @param ts_individual A single time series node.
transmission_parse_timeseries_individual <- function(ts_individual){

  ts_indi_list <- list()

  ts_indi_list$mrid <-
    ts_individual %>%
    rvest::html_node(xpath = "mrid") %>%
    rvest::html_text()

  ts_indi_list$businesstype <-
    ts_individual %>%
    rvest::html_node(xpath = "businesstype") %>%
    rvest::html_text()

  ts_indi_list$in_domain.mrid <-
    try(ts_individual %>%
          rvest::html_node(xpath = "in_domain.mrid") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$out_domain.mrid <-
    try(ts_individual %>%
          rvest::html_node(xpath = "out_domain.mrid") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$quantity_measure_unit.name <-
    try(ts_individual %>%
          rvest::html_node(xpath = "quantity_measure_unit.name") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$currency_unit.name <-
    try(ts_individual %>%
          rvest::html_node(xpath = "currency_unit.name") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$mktpsrtype.psrtype <-
    try(ts_individual %>%
          rvest::html_node(xpath = "mktpsrtype.psrtype") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$curvetype <-
    ts_individual %>%
    rvest::html_node(xpath = "curvetype") %>%
    rvest::html_text()

  ts_indi_list$end_dateandortime.date <-
    try(ts_individual %>%
          rvest::html_node(xpath = "end_dateandortime.date") %>%
          rvest::html_text(), silent = TRUE)

  ts_indi_list$flowdirection.direction <-
    try(ts_individual %>%
          rvest::html_node(xpath = "flowdirection.direction") %>%
          rvest::html_text(), silent = TRUE)

  # I dont parse MBIE asset_registeredresources
  # I dont parse MBIE reason


  ts_indi_df <- tibble::as_tibble(ts_indi_list)

  if(ts_indi_df$curvetype == "A01"){
    ts_indi_df$points <- list(transmission_parse_period(ts_individual))
  }

  ts_indi_df
}
