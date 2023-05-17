#' @import dplyr
#' @import ggplot2
#' @import cli
#' @importFrom rappdirs user_cache_dir
.onAttach <- function(libname, pkgname) {
  print_logo()
}
