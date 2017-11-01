

entsoe_create_url <- function(documentType = NULL,
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
                              securityToken = NULL){
  
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
  
  final_url
}