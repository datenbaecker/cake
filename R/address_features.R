
#' @export
get_address_features <- function(addresses = NULL, data_provider = default_data_provider()) {
  req_body <- lapply(addresses, function(x) list(full_address = x, date = as.character(Sys.Date())))
  post_res <- order_and_serve(
    "address-features",
    body_json = req_body,
    data_provider,
    read_body_hook = extract_parquet_response
  )
  post_res
}
