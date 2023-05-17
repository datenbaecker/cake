#
# coordinates <- function(lat, lon, srid = "4326") {
#
# }
#
# address_strings <- function(addr, geocoder) {
#
# }
#
# address_components <- function(street, number, zip_code, canton, geocoder) {
#
# }
#
# swiss_communes <- function(identifier_column, type = "bfs_number") {
#
# }
#
# swiss_cantons <- function(identifier_column, type = "bfs_number") {
#
# }
#
# create_payload <- function(data, spatial_columns) {
#   UseMethod("create_payload", spatial_columns)
# }
#
# create_payload.coordinates <- function(data, spatial_columns) {
#
# }
#
# serve <- function(data, spatial_columns, provider = datenbaecker()) {
#
# }

menu <- function(dataprovider) {
  UseMethod("menu", dataprovider)
}

menu.local_datacake_provider <- function(dataprovider) {
  all_files <- list.files(dataprovider$resource_folder)
  basenames <- tools::file_path_sans_ext(all_files)
  ret <- unique(basenames) %>%
    lapply(function(x) tools::file_ext(all_files[basenames == x]))
  names(ret) <- unique(basenames)
  ret
}

menu.datenbaecker <- function(dataprovider) {
  list(
    "swiss_boundaries" = c("rds")
  )
}

serve <- function(dataprovider, what, flavor) {
  UseMethod("serve", dataprovider)
}

serve.local_datacake_provider <- function(dataprovider, what, flavor) {

}

serve.datenbaecker <- function(dataprovider, what, flavor) {

}

download_swiss_boundaries <- function(url) {
  res <- httr::GET("file:///home/luki/projekte/css/swiss_boundaries.rds")
  file_conn <- httr::content(res) %>%
    rawConnection() %>%
    gzcon()
  boundaries <- readRDS(file_conn)
  close(file_conn)
  boundaries
}

swiss_boundaries <- function() {
  sb <- NULL
  function() {
    if (is.null(sb)) {
      sb <<- readRDS("/home/luki/projekte/css/swiss_boundaries.rds")
      if ("bfs_nummer" %in% names(sb)) {
        sb <<- sb %>%
          rename(bfs_num = bfs_nummer)
      }
    }
    sb
  }
}
get_swiss_boundaries <- swiss_boundaries()


