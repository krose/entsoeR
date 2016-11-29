

load_parse_doc_header <- function(e_content, doc_name){

  doc_head <- list()

  doc_head$mrid <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "mrid") %>%
    rvest::html_text()

  doc_head$revisionNumber <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "revisionnumber") %>%
    rvest::html_text()

  doc_head$type <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "type") %>%
    rvest::html_text()

  doc_head$process.processtype <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "process.processtype") %>%
    rvest::html_text()

  doc_head$createddatetime <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "createddatetime") %>%
    rvest::html_text()

  doc_head$sender_marketparticipant.mrid <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "sender_marketparticipant.mrid") %>%
    rvest::html_text()

  doc_head$sender_marketparticipant.marketrole.type <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "sender_marketparticipant.marketrole.type") %>%
    rvest::html_text()

  doc_head$receiver_marketparticipant.mrid <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "receiver_marketparticipant.mrid") %>%
    rvest::html_text()

  doc_head$receiver_marketparticipant.marketrole.type <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "receiver_marketparticipant.marketrole.type") %>%
    rvest::html_text()

  doc_head$time_period.timeinterval.start <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "time_period.timeinterval") %>%
    rvest::html_node(xpath = "start") %>%
    rvest::html_text()

  doc_head$time_period.timeinterval.end <-
    e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "time_period.timeinterval") %>%
    rvest::html_node(xpath = "end") %>%
    rvest::html_text()


  doc_head$docstatus <-
    try(e_content %>%
    rvest::html_node(doc_name) %>%
    rvest::html_nodes(xpath = "receiver_marketparticipant.marketrole.type") %>%
    rvest::html_text(), silent = TRUE)

  tibble::as_tibble(doc_head)
}
