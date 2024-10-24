
#' Get Analytical Base Table for Swiss Addresses
#'
#' @param addresses Either a character vector with address strings to
#' search for or \code{NULL} for a standard address sample
#' @param data_provider An object of type \code{data_provider}
#' (see \code{\link[cake]{datenbaecker}})
#'
#' @description This function provides a set of variables for Swiss addresses.
#' The data originates from different sources.
#' You can call \code{\link[cake]{metadata}} on the data frame in order to get
#' a description and the source of the data in the columns.
#' If \code{NULL} is passed for the argument \code{addresses}, you get the data
#' for a selected sample of Swiss addresses.
#' If address strings are provided, the backend does a fuzzy matching with
#' the addresses from the
#' \href{https://www.swisstopo.admin.ch/en/official-directory-of-building-addresses}{official directory of building addresses}
#' from swisstopo.
#'
#'
#' @return A table with a battery of variables for the addresses.
#' @export
#'
#' @examples
#'\dontrun{
#' # This code depends on the availability of the remote server
#' dp <- datenbaecker(cache_dir = FALSE)
#' search_for <- c("Bundesstrasse 9 6003 Luzern", "Rämistrasse 71 8006 Zürich")
#' adr_data <- get_address_features(search_for, data_provider = dp)
#'}
get_address_features <- function(addresses = NULL, data_provider = default_data_provider()) {
  if (is.null(addresses)) {
    data <- serve(
      "address-features/standard-dataset", data_provider, read_body_hook = extract_parquet_response,
      metadata_endpoint="address-features"
    )
  } else {
    req_body <- lapply(addresses, function(x) list(full_address = x, date = as.character(Sys.Date())))
    data <- order_and_serve(
      "address-features",
      body_json = req_body,
      data_provider,
      read_body_hook = extract_parquet_response,
      timeout = 120
    )
  }
  data
}
