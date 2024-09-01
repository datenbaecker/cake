
#' @export
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
      read_body_hook = extract_parquet_response
    )
  }
  data
}
