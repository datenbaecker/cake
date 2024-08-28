#' @import dplyr
#' @import ggplot2
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom rappdirs user_cache_dir
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_url_path_append
#' @importFrom httr2 req_url_query
#' @importFrom httr2 req_body_json
#' @importFrom httr2 resp_body_raw
#' @importFrom httr2 resp_body_json
#' @importFrom qs qread
#' @importFrom qs qsave
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf "st_crs<-"
#' @importFrom sf st_bbox
#' @importFrom arrow read_parquet
.onAttach <- function(libname, pkgname) {

}
