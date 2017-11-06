




#' This wraps a GET request to the API
#'
#' @param documentType
#' @param processType
#' @param businessType
#' @param psrType
#' @param type_MarketAgreement.Type
#' @param auction.Type
#' @param auction.Category
#' @param classificationSequence_AttributeInstanceComponent.Position
#' @param outBiddingZone_Domain
#' @param biddingZone_Domain
#' @param controlArea_Domain
#' @param in_Domain
#' @param out_Domain
#' @param acquiring_Domain
#' @param timeInterval
#' @param periodStart
#' @param periodEnd
#' @param securityToken
#' @export
#' @examples 
#' 
#' library(entsoeR)
#' 
#' # Actual Total outages
#' # https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_outages
#' # 4.6.1. Unavailability of Consumption Units [7.1A&B]
#' outages_get(documentType = "A76", 
#'             periodStart = "201702012300", 
#'             periodEnd = "201702172300", 
#'             outBiddingZone_Domain = "10YCZ-CEPS-----N")
#'  
#'  # 4.6.2. Unavailability of Transmission Infrastructure [10.1.A&B]
#'  outages_get(documentType = "A78", 
#'              businessType = "A53",
#'              in_Domain = "10YCZ-CEPS-----N",
#'              out_Domain = "10YSK-SEPS-----K",
#'              periodStart = "201702162300", 
#'              periodEnd = "201702192300")
#'           
#'  # 4.6.3. Unavailability of Offshore Grid Infrastructure [10.1.C]
#'  outages_get(documentType = "A79", 
#'           biddingZone_Domain = "10YDE-EON------1",
#'           periodStart = "201512312300", 
#'           periodEnd = "201612312300")
#'  
#'  # 4.6.4. Unavailability of Generation Units [15.1.A&B]
#'  outages_get(documentType = "A80",
#'              businessType = "A53",
#'              biddingZone_Domain = "10YCZ-CEPS-----N",
#'              periodStart = "201512312300", 
#'              periodEnd = "201612312300")
#'              
#'  4.6.5. Unavailability of Production Units [15.1.C&D]
#'  outages_get(documentType = "A77",
#'              businessType = "A53",
#'              biddingZone_Domain = "10YCZ-CEPS-----N",
#'              periodStart = "201512312300", 
#'              periodEnd = "201612312300")
#' 
outages_get <- function(  documentType = NULL,
                       processType = NULL,
                       businessType = NULL,
                       psrType = NULL,
                       type_MarketAgreement.Type = NULL,
                       contract_MarketAgreement.Type = NULL,
                       auction.Type = NULL,
                       auction.Category = NULL,
                       classificationSequence_AttributeInstanceComponent.Position = NULL,
                       outBiddingZone_Domain = NULL,
                       biddingZone_Domain = NULL,
                       controlArea_Domain = NULL,
                       in_Domain = NULL,
                       out_Domain = NULL,
                       acquiring_Domain = NULL,
                       timeInterval = NULL,
                       periodStart = NULL,
                       periodEnd = NULL,
                       securityToken = Sys.getenv("ENTSOE_PAT")){
  
  final_url <- entsoe_create_url(documentType = documentType,
                                 processType = processType,
                                 businessType = businessType,
                                 psrType = psrType,
                                 type_MarketAgreement.Type = type_MarketAgreement.Type,
                                 contract_MarketAgreement.Type = contract_MarketAgreement.Type,
                                 auction.Type = auction.Type,
                                 auction.Category = auction.Category,
                                 classificationSequence_AttributeInstanceComponent.Position = classificationSequence_AttributeInstanceComponent.Position,
                                 outBiddingZone_Domain = outBiddingZone_Domain,
                                 biddingZone_Domain = biddingZone_Domain,
                                 controlArea_Domain = controlArea_Domain,
                                 in_Domain = in_Domain,
                                 out_Domain = out_Domain,
                                 acquiring_Domain = acquiring_Domain,
                                 timeInterval = timeInterval,
                                 periodStart = periodStart,
                                 periodEnd = periodEnd,
                                 securityToken = securityToken)
  
  # make GET request
  tempfile_path <- tempfile()
  tempdir_path <- tempdir()
  e_request <- httr::GET(url = final_url, httr::write_disk(tempfile_path))

  # check status
  if(httr::status_code(e_request) != 200){
    stop(paste0(httr::http_status(e_request)$category, ". ",
                httr::http_status(e_request)$reason, ". ",
                httr::http_status(e_request)$message, ". ",
                e_request %>% httr::content(., encoding = "UTF-8") %>% 
                  xml2::xml_child(., 8) %>% 
                  xml2::xml_child(., 2) %>% 
                  xml2::xml_text()))
  }
    
  # Check if the get request returns application/zip
  # if yes, save to folder and unzip
  # else, parse the file.
  if(httr::http_type(e_request) == "application/zip"){
    
    zip_files <- unzip(tempfile_path, list = TRUE)
    zip_files$path <- paste0(tempdir_path, "/", zip_files$Name)
    
    unzip(zipfile = tempfile_path, exdir = tempdir_path)
    
    unzipped_files <- purrr::map(zip_files$path, ~xml2::read_html(.x, encoding = "UTF-8"))
    
    e_content <- purrr::map(unzipped_files, ~outages_helper(.x))
    e_content <- dplyr::bind_rows(e_content)
    
  } else if(httr::http_type(e_request) == "application/xml"){
    
    e_content <- httr::content(e_request, encoding = "UTF-8")
    
  } else {
    
    stop("Http type not supported.", call. = FALSE)
  }

  e_content
}


outages_helper <- function(html_doc){
  
  html_doc <- html_doc %>% rvest::html_node("unavailability_marketdocument")
  
  ids <- c("mRID", 
           "revisionNumber", 
           "type", 
           "process.processType", 
           "createdDateTime", 
           "sender_MarketParticipant.mRID", 
           "sender_MarketParticipant.marketRole.type", 
           "receiver_MarketParticipant.mRID", 
           "receiver_MarketParticipant.marketRole.type", 
           "unavailability_Time_Period.timeInterval",
           "docStatus")
  ids <- tolower(ids)
  
  id_extractor <- function(html_doc, id){
    
    rvest::html_nodes(html_doc, xpath = id) %>%
      rvest::html_text() %>%
      tibble::tibble(id = id, value = .)
  }
  
  doc_result <- 
    purrr::map(ids, ~id_extractor(html_doc, .x)) %>% 
    dplyr::bind_rows() %>%
    tidyr::spread(id, value) %>%
    dplyr::mutate_all(dplyr::funs(readr::parse_guess(.)))
  
  ####################################
  # parse timeseries
  ######################################
  
  ids <- c("mRID",
           "businessType",
           "biddingZone_Domain.mRID",
           "in_Domain.mRID",
           "out_Domain.mRID",
           "start_DateAndOrTime.date",
           "start_DateAndOrTime.time",
           "end_DateAndOrTime.date",
           "end_DateAndOrTime.time",
           "quantity_Measure_Unit.name",
           "curveType",
           "production_RegisteredResource.mRID",
           "production_RegisteredResource.name",
           "production_RegisteredResource.location.name",
           "production_RegisteredResource.pSRType.psrType",
           "production_RegisteredResource.pSRType.powerSystemResources.mRID",
           "production_RegisteredResource.pSRType.powerSystemResources.name",
           "production_RegisteredResource.pSRType.powerSystemResources.nominalP")
  ids <- tolower(ids)
  
  html_ts <- 
    html_doc %>% 
    rvest::html_nodes("timeseries")
  
  doc_result_ts <- 
    purrr::map(ids, ~id_extractor(html_ts, .x)) %>% 
    dplyr::bind_rows() %>%
    tidyr::spread(id, value) %>%
    dplyr::mutate_all(dplyr::funs(readr::parse_guess(.)))
  
  doc_result$timeseries <- list(doc_result_ts)
  
  ids <- c("timeInterval", 
           "resolution")
  ids <- tolower(ids)
  
  html_ts_ps <- 
    html_ts %>% 
    rvest::html_nodes("available_period")
  
  doc_result_ts_ps <- 
    purrr::map(ids, ~id_extractor(html_ts_ps, .x)) %>% 
    dplyr::bind_rows() %>%
    tidyr::spread(id, value) %>%
    dplyr::mutate_all(dplyr::funs(readr::parse_guess(.)))
  
  doc_result$point_series <- list(doc_result_ts_ps)
  
  ids <- c("position", 
           "quantity")
  ids <- tolower(ids)
  
  html_ts_ps_p <- 
    html_ts_ps %>% 
    rvest::html_nodes("point")
  
  doc_result_ts_ps_p <- 
    purrr::map(ids, ~id_extractor(html_ts_ps_p, .x)) %>% 
    dplyr::bind_rows() %>%
    tidyr::spread(id, value) %>%
    dplyr::mutate_all(dplyr::funs(readr::parse_guess(.)))
  
  doc_result$point <- list(doc_result_ts_ps_p)
  
  ##########################################
  # parse reason
  #############################################
  
  ids <- c("code", "text")
  
  html_reason <- 
    html_doc %>% 
    rvest::html_nodes("reason")
  
  doc_result_reason <- 
    purrr::map(ids, ~id_extractor(html_reason, .x)) %>% 
    dplyr::bind_rows() %>%
    tidyr::spread(id, value)
  
  doc_result$reason <- list(doc_result_reason)
  doc_result <- tidyr::unnest(doc_result, reason, .sep = "_")

  doc_result
}




