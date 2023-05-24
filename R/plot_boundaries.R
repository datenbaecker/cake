
StatBoundary <- ggplot2::ggproto(
  "StatBoundary", ggplot2::Stat,
  compute_layer = function(self, data, params, layout) {
    # This is necessary because of a bug in ggplot2 <= 3.4.1
    # see issue 4284 https://github.com/tidyverse/ggplot2/issues/4284
    if ("id" %in% names(data) && !is.numeric(data[["id"]])) {
      data[["id"]] <- as.factor(data[["id"]])
    }
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  },
  compute_panel = function(data, scales, data_provider = NULL, gov_level = "canton") {
    if (is.null(data_provider)) {
      datacake_abort("dataprovider_required")
    }
    cols_to_keep <- setdiff(names(data), c("x", "y", "group", "subgroup"))
    data_keep <- data[, cols_to_keep] %>%
      unique()
    sb <- serve("swiss_boundaries", data_provider) %>%
      filter(entity == gov_level)
    if (!is.numeric(data_keep$id)) {
      sb <- sb %>%
        mutate(id = label)
    } else {
      sb <- sb %>%
        mutate(id = bfs_num)
    }
    ret <- sb %>%
      select(id, x, y, group, subgroup, bfs_num) %>%
      inner_join(data_keep, by = "id", suffix = c("", "_y"))
    if (!"fill" %in% cols_to_keep) {
      ret <- ret %>%
        mutate(fill = bfs_num)
    }
    ret
  },
  required_aes = c("id")
)

#' @title Plot Swiss Boundaries
#' @rdname geom_swiss_boundaries
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}.
#' @param data Data with the units for plotting
#' @param data_provider A datacake data provider.
#' @param position
#' @param ...
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param gov_level Goverment level (either \dQuote{commune} or \dQuote{canton})
#'
#' @section Aesthetics:
#' The only required aesthetics is \code{id} and should contain the
#' identification for the geographical unit (canton or commune).
#' If the column is numeric, the values get interpreted as
#' or canton number according to the definition of the Swiss
#' Federal Statistical Office (see \url{https://www.bfs.admin.ch/bfs/de/home/grundlagen/agvch/identifikatoren-gemeinde.html}).
#' Otherwise the column gets interpreted as commune name or
#' abbreviated canton (ZH, BE, LU, \dots).
#' The aesthetics \code{x}, \code{y}, \code{group} and \code{subgroup} are ignored.
#' Otherwise the function supports the same aesthetics as \code{\link[ggplot2]{geom_polygon}}
#'
#' @examples
#' \dontrun{
#' dp <- LocalDatacakeProvider("<path/to/provider>")
#' cantons <- tibble(label = c("LU", "ZH"))
#' ggplot(cantons, aes(id = label)) + geom_canton(data_provider = dp)
#' }
#'
#'
#' @export
#'
geom_swiss_boundaries <- function(mapping = NULL, data = NULL, data_provider = NULL, position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, gov_level = c("canton", "commune")) {
  gov_level <- match.arg(gov_level, c("canton", "commune"))
  layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = gov_level, data_provider = data_provider, ...))
}

#' @rdname geom_swiss_boundaries
#' @export
geom_canton <- function(mapping = NULL, data = NULL, data_provider = NULL, position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = "canton", data_provider = data_provider, ...))

}

#' @rdname geom_swiss_boundaries
#' @export
geom_commune <- function(mapping = NULL, data = NULL, data_provider = NULL, position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = "commune", data_provider = data_provider, ...))

}
