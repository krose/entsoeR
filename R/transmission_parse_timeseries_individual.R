

#' Helper function to parse a single time series node.
#'
#' The time series structure can be individual so I need to add the
#' possible nodes that can pop-up.
#'
#' @param ts_individual A single time series node.
transmission_parse_timeseries_individual <- function(ts_individual){

  ts_individual <-
    ts_individual %>%
    rvest::xml_children()

  ts_indi_list <- list()

  ts_indi_list$mrid <-
    ts_individual %>%
    .[["mrid"]] %>%
    rvest::xml_text()

  ts_indi_list$businesstype <-
    ts_individual %>%
    .[["businesstype"]] %>%
    rvest::xml_text()

  ts_indi_list$in_domain.mrid <-
    try(ts_individual %>%
          .[["in_domain.mrid"]] %>%
          rvest::xml_text(), silent = TRUE)

  ts_indi_list$out_domain.mrid <-
    try(ts_individual %>%
          .[["out_domain.mrid"]] %>%
          rvest::xml_text(), silent = TRUE)

  ts_indi_list$quantity_measure_unit.name <-
    try(ts_individual %>%
          .[["quantity_measure_unit.name"]] %>%
          rvest::xml_text(), silent = TRUE)

  ts_indi_list$currency_unit.name <-
    try(ts_individual %>%
          .[["currency_unit.name"]] %>%
          rvest::xml_text(), silent = TRUE)

  ts_indi_list$mktpsrtype.psrtype <-
    try(ts_individual %>%
          .[["mktpsrtype.psrtype"]] %>%
          rvest::xml_text(), silent = TRUE)

  ts_indi_list$curvetype <-
    ts_individual %>%
    .[["curvetype"]] %>%
    rvest::xml_text()

  ts_indi_list$end_dateandortime.date <-
    try(ts_individual %>%
          .[["end_dateandortime.date"]] %>%
          rvest::xml_text(), silent = TRUE)

  ts_indi_list$flowdirection.direction <-
    try(ts_individual %>%
          .[["flowdirection.direction"]] %>%
          rvest::xml_text(), silent = TRUE)

  # I dont parse MBIE asset_registeredresources
  # I dont parse MBIE reason

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
    ts_indi_df$points <- list(transmission_parse_period(ts_individual))
  }

  ts_indi_df
}
