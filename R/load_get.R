


#' This wraps a GET request to the API
#'
#' @param documentType The coded type of the document being sent. The 
#'     document type identifies the principal characteristic of the 
#'     status request. Refer to ENTSO-E 
#'     Core Component Code list document for valid codes. The document 
#'     type value must be exactly 3 alphanumeric characters (no blanks).
#'     This information is mandatory. There's no dependency 
#'     requirements.
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
#' # Actual Total Load
#' # https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_load_domain
#' # 4.1.1
#' load_get(documentType = "A65", 
#'          processType = "A16", 
#'          periodStart = "201702012300", 
#'          periodEnd = "201702172300", 
#'          outBiddingZone_Domain = "10YCZ-CEPS-----N")
#'  
#'  # 4.1.2. Day-Ahead Total Load Forecast [6.1.B]
#'  load_get(documentType = "A65", 
#'           processType = "A01", 
#'           periodStart = "201702162300", 
#'           periodEnd = "201702192300", 
#'           outBiddingZone_Domain = "10YCZ-CEPS-----N")
#'           
#'  # 4.1.3. Week-Ahead Total Load Forecast [6.1.C]
#'  # This returns a wierd result, see the second
#'  # call where there are two different business types.
#'  load_get(documentType = "A65", 
#'           processType = "A32", 
#'           periodStart = "201702162300", 
#'           periodEnd = "201703192300", 
#'           outBiddingZone_Domain = "10YCZ-CEPS-----N")
#'  load_get(documentType = "A65", 
#'           processType = "A32", 
#'           periodStart = "201512312300", 
#'           periodEnd = "201612312300", 
#'           outBiddingZone_Domain = "10YCZ-CEPS-----N", return_all = TRUE)
#'  
#'  # 4.1.5. Year-Ahead Total Load Forecast [6.1.E]
#'  # NOT WORKING resolution not implemented.
#'  load_get(documentType = "A65", 
#'           processType = "A33", 
#'           periodStart = "201612312300", 
#'           periodEnd = "201712312300", 
#'           outBiddingZone_Domain = "10YCZ-CEPS-----N")
#' 
load_get <- function(  documentType = NULL,
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
  if(httr::status_code(e_request) != 200){
    stop(paste0(httr::http_status(e_request)$category, ". ", httr::http_status(e_request)$reason, ". ", httr::http_status(e_request)$message, ". "))
  }

  e_content <- httr::content(x = e_request, as = "text", encoding = "UTF-8")
  e_content <- xml2::read_html(e_content, encoding = "UTF-8")

  doc_name <-
    e_content %>%
    rvest::html_node("body") %>%
    xml2::xml_children() %>%
    xml2::xml_name()

  doc_header <- load_parse_doc_header(e_content = e_content, doc_name = doc_name)
  timeseries <- load_parse_timeseries(e_content = e_content, doc_name = doc_name)

  list(doc_header = doc_header, time_series = timeseries)
}







