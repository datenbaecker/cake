
#' Search for Swiss Companies by Name
#'
#' @param companies A character vector with company names
#' @param data_provider An object of type \code{data_provider}
#' (see \code{\link[cake]{datenbaecker}})
#'
#' @description The function sends a request with the searched companies to the backend.
#' The backend performs a fuzzy matching with the company name.
#'
#' @return A list with:
#' * \code{company_details}: A table with information for the searched companies (legal form, address, etc.)
#' * \code{related_companies}: A table with related companies for the search companies
#' @export
#'
#' @examples
#'\dontrun{
#' # This code depends on the availability of the remote server
#' dp <- datenbaecker(cache_dir = FALSE)
#' search_for <- c("Datenbäcker")
#' company_data <- search_companies(search_for, data_provider = dp)
#'}
search_companies <- function(companies, data_provider = default_data_provider()) {
  req_body <- lapply(companies, function(x) list(company_name = x, date = as.character(Sys.Date())))
  data <- order_and_serve(
    "company-search",
    body_json = req_body,
    data_provider,
    read_body_hook = extract_parquet_response,
    timeout = 120
  )
  names(data) <- c("company_details", "related_companies")
  data$related_companies <- data$related_companies %>%
    filter(!is.na(related_uid))
  data
}
