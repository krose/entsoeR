


#' This wraps a GET request to the API
#'
#' @param securityToken
#' @param documentType
#' @param processType
#' @param outBiddingZone_Domain
#' @param periodStart
#' @param periodEnd
#' @export
entsoe_get <- function(securityToken = NULL,
                       documentType = "A65",
                       processType = "A16",
                       outBiddingZone_Domain = "10YCZ-CEPS-----N",
                       periodStart = "201512312300",
                       periodEnd =   "201612312300"){

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
  if(!is.null(outBiddingZone_Domain)) final_url <- paste0(final_url, "&outBiddingZone_Domain=", outBiddingZone_Domain)
  if(!is.null(periodStart)) final_url <- paste0(final_url, "&periodStart=", periodStart)
  if(!is.null(periodEnd)) final_url <- paste0(final_url, "&periodEnd=", periodEnd)

  # make GET request
  e_request <- httr::GET(url = final_url)

  # check status
  # httr::http_status(x = e_request)

  e_content <- httr::content(x = e_request, as = "text")
  e_content <- xml2::read_xml(e_content)
  e_content <- xml2::as_list(e_content)

  e_content
}







