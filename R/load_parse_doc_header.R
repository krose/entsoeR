

load_parse_doc_header <- function(e_content, doc_name){

  doc_head <- list()

  doc_head$mrid <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "mrid") %>%
    rvest::xml_text()

  doc_head$revisionNumber <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "revisionnumber") %>%
    rvest::xml_text()

  doc_head$type <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "type") %>%
    rvest::xml_text()

  doc_head$process.processtype <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "process.processtype") %>%
    rvest::xml_text()

  doc_head$createddatetime <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "createddatetime") %>%
    rvest::xml_text()

  doc_head$sender_marketparticipant.mrid <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "sender_marketparticipant.mrid") %>%
    rvest::xml_text()

  doc_head$sender_marketparticipant.marketrole.type <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "sender_marketparticipant.marketrole.type") %>%
    rvest::xml_text()

  doc_head$receiver_marketparticipant.mrid <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "receiver_marketparticipant.mrid") %>%
    rvest::xml_text()

  doc_head$receiver_marketparticipant.marketrole.type <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "receiver_marketparticipant.marketrole.type") %>%
    rvest::xml_text()

  doc_head$time_period.timeinterval.start <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "time_period.timeinterval") %>%
    rvest::xml_node(xpath = "start") %>%
    rvest::xml_text()

  doc_head$time_period.timeinterval.end <-
    e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "time_period.timeinterval") %>%
    rvest::xml_node(xpath = "end") %>%
    rvest::xml_text()


  doc_head$docstatus <-
    try(e_content %>%
    rvest::xml_node(doc_name) %>%
    rvest::xml_nodes(xpath = "receiver_marketparticipant.marketrole.type") %>%
    rvest::xml_text(), silent = TRUE)

  tibble::as_tibble(doc_head)
}
