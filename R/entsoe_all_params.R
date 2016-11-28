
#' Function to download all params.
#'
#' @export
entsoe_all_params <- function(){

  entsoe_url <- "https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html"

  # subset the relevant lists
  entsoe_vars <-
    rvest::html(entsoe_url) %>%
    rvest::html_nodes(".sect1:nth-child(5) table") %>%
    rvest::html_table(header = TRUE) %>%
    .[c(1, 4:12)]

  # Get the table names
  names(entsoe_vars) <-
    rvest::html(entsoe_url) %>%
    rvest::html_nodes("#_complete_parameter_list+ .sectionbody h3") %>%
    rvest::html_text()

  entsoe_vars
}
