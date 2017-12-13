


id_extractor <- function(html_doc, ids) {
  
  r <- rvest::html_nodes(html_doc, xpath = paste(ids, collapse = "|"))
  
  as.list(r %>% rvest::html_text()) %>% 
    purrr::set_names(r %>% rvest::html_name()) %>% 
    tibble::as_tibble()
}


