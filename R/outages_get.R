




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
  

  # Build url
  base_url <- "https://transparency.entsoe.eu/api?"
  
  final_url <- paste0(base_url, "securityToken=", securityToken)
  
  if(!is.null(documentType)) final_url <- paste0(final_url, "&documentType=", documentType)
  if(!is.null(processType)) final_url <- paste0(final_url, "&processType=", processType)
  if(!is.null(businessType)) final_url <- paste0(final_url, "&businessType=", businessType)
  if(!is.null(psrType)) final_url <- paste0(final_url, "&psrType=", psrType)
  if(!is.null(type_MarketAgreement.Type)) final_url <- paste0(final_url, "&type_MarketAgreement.Type=", type_MarketAgreement.Type)
  if(!is.null(contract_MarketAgreement.Type)) final_url <- paste0(final_url, "&contract_MarketAgreement.Type=", contract_MarketAgreement.Type)
  if(!is.null(auction.Type)) final_url <- paste0(final_url, "&auction.Type=", auction.Type)
  if(!is.null(auction.Category)) final_url <- paste0(final_url, "&auction.Category=", auction.Category)
  if(!is.null(classificationSequence_AttributeInstanceComponent.Position)){
    final_url <- paste0(final_url,
                        "&classificationSequence_AttributeInstanceComponent.Position=",
                        classificationSequence_AttributeInstanceComponent.Position)
  }
  if(!is.null(outBiddingZone_Domain)) final_url <- paste0(final_url, "&outBiddingZone_Domain=", outBiddingZone_Domain)
  if(!is.null(biddingZone_Domain)) final_url <- paste0(final_url, "&biddingZone_Domain=", biddingZone_Domain)
  if(!is.null(controlArea_Domain)) final_url <- paste0(final_url, "&controlArea_Domain=", controlArea_Domain)
  if(!is.null(in_Domain)) final_url <- paste0(final_url, "&in_Domain=", in_Domain)
  if(!is.null(out_Domain)) final_url <- paste0(final_url, "&out_Domain=", out_Domain)
  if(!is.null(acquiring_Domain)) final_url <- paste0(final_url, "&acquiring_Domain=", acquiring_Domain)
  if(!is.null(timeInterval)) final_url <- paste0(final_url, "&timeInterval=", timeInterval)
  if(!is.null(periodStart)) final_url <- paste0(final_url, "&periodStart=", periodStart)
  if(!is.null(periodEnd)) final_url <- paste0(final_url, "&periodEnd=", periodEnd)
  
  # make GET request
  tempfile_path <- tempfile()
  tempdir_path <- tempdir()
  e_request <- httr::GET(url = final_url, httr::write_disk(tempfile_path))
  
  zip_files <- unzip(tempfile_path, list = TRUE)
  zip_files$path <- paste0(tempdir_path, "/", zip_files$Name)
  
  unzip(zipfile = tempfile_path, exdir = tempdir_path)
  
  unzipped_files <- purrr::map(zip_files$path, ~xml2::read_html(.x, encoding = "UTF-8"))
  
  unzipped_files <- purrr::map(unzipped_files, ~outages_helper(.x))
  # # check status
  # if(httr::status_code(e_request) != 200){
  #   stop(paste0(httr::http_status(e_request)$category, ". ", 
  #               httr::http_status(e_request)$reason, ". ", 
  #               httr::http_status(e_request)$message, ". "))
  # }
  # 
  # e_content <- httr::content(x = e_request, encoding = "UTF-8")
  # e_content <- xml2::read_html(e_content, encoding = "UTF-8")
  # 
  # doc_name <-
  #   e_content %>%
  #   rvest::html_node("body") %>%
  #   xml2::xml_children() %>%
  #   xml2::xml_name()
  
  unzipped_files
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
  
  id_extractor <- function(html_doc, id){
    
    rvest::html_nodes(html_doc, id) %>%
      rvest::html_text() %>%
      tibble::tibble(id = id, value = .)
  }
  
  purrr::map(ids, ~id_extractor(html_doc, .x)) %>% 
    bind_rows()
}




