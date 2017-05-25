
#' Get the Energy Identification Codes (EIC)
#' 
#' Read more about the EIC here: https://www.entsoe.eu/fileadmin/user_upload/edi/library/downloads/EIC_Reference_Manual.pdf
#'
#' @export
#' 
#' @examples 
#' library(entsoeR)
#' 
#' eic()
eic <- function(){
  entso_xml_link <- "https://www.entsoe.eu/fileadmin/user_upload/edi/library/eic/allocated-eic-codes.xml"
  
  xml_data <- xml2::read_html(entso_xml_link)
  
  xml_mkt_doc <- 
    xml_data %>%
    rvest::html_nodes("body eiccode_marketdocument")
  
  eic_xml <- 
    tibble::tibble(
      
      mrid = 
        xml_mkt_doc %>%
        rvest::html_nodes("mrid") %>%
        rvest::html_text(),
      
      docstatus_value = 
        xml_mkt_doc %>%
        rvest::html_nodes("docstatus value") %>%
        rvest::html_text(),
      
      attributeinstancecomponent = 
        xml_mkt_doc %>%
        rvest::html_nodes(xpath = "attributeinstancecomponent.attribute") %>%
        rvest::html_text(),
      
      long_names_name = 
        xml_mkt_doc %>%
        rvest::html_nodes(xpath = "long_names.name") %>%
        rvest::html_text(),
      
      display_names_name = xml_mkt_doc %>%
        rvest::html_nodes(xpath = "display_names.name") %>%
        rvest::html_text(),
      
      lastrequest_dateandortime_date = 
        xml_mkt_doc %>%
        rvest::html_nodes(xpath = "lastrequest_dateandortime.date") %>%
        rvest::html_text(),
      
      deactivationrequested_dateandortime_date = 
        xml_mkt_doc %>%
        rvest::html_nodes(xpath = "deactivationrequested_dateandortime.date") %>%
        rvest::html_text(),
      
      eiccode_marketparticipant_streetadress_country = 
        xml_mkt_doc %>%
        rvest::html_nodes(xpath = "eiccode_marketparticipant.streetaddress") %>%
        rvest::html_node("towndetail country") %>%
        rvest::html_text(),
      
      eiccode_marketparticipant_acercode_names_name = 
        xml_mkt_doc %>%
        rvest::html_nodes(xpath = "eiccode_marketparticipant.acercode_names.name") %>%
        rvest::html_text(),
      
      description = 
        xml_mkt_doc %>%
        rvest::html_node("description") %>%
        rvest::html_text(),
      
      function_names = 
        xml_mkt_doc %>%
        rvest::html_node("function_names name") %>%
        rvest::html_text()
      
    )
  
  eic_xml
}








