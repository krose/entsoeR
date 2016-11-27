


#' This wraps a GET request to the API
#'
#' @param securityToken
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
#' @export
load_get <- function(securityToken = NULL,
                       documentType = NULL,
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
                       return_all = FALSE){

  # base_url <- list(
  #   scheme = "https",
  #   hostname = "transparency.entsoe.eu/api",
  #   path = list(
  #     securityToken = securityToken,
  #     processType = processType,
  #     outBiddingZone_Domain = outBiddingZone_Domain,
  #     periodStart = periodStart,
  #     periodEnd = periodEnd
  #   )
  # )
  #
  # final_url <- httr::build_url(base_url)

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
  e_request <- httr::GET(url = final_url)

  # check status
  # httr::http_status(x = e_request)

  e_content <- httr::content(x = e_request, as = "text")
  e_content <- rvest::xml(e_content, encoding = "UTF")

  doc_name <-
    e_content %>%
    rvest::xml_node("body") %>%
    rvest::xml_children() %>%
    names()

  doc_header <- load_parse_doc_header(e_content = e_content, doc_name = doc_name)
  timeseries <- load_parse_timeseries(e_content = e_content, doc_name = doc_name)

  if(return_all){
    return(list(doc_header = doc_header, time_series = timeseries))
  } else {

    return(timeseries[["points"]] %>%
      dplyr::bind_rows())
  }
  e_content
}







