
StatWkt <- ggplot2::ggproto(
  "StatWkt", ggplot2::Stat,
  compute_layer = function(self, data, params, layout) {
    # This is necessary because of a bug in ggplot2 <= 3.4.1
    # see issue 4284 https://github.com/tidyverse/ggplot2/issues/4284
    if ("id" %in% names(data) && !is.numeric(data[["id"]])) {
      data[["id"]] <- as.factor(data[["id"]])
    }
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  },
  compute_panel = function(data, scales, data_provider = NULL, product = "geometries/plz-shape", read_body_hook = identity, col_name_wkt = "shape_wkt") {
    if (is.null(data_provider)) {
      datacake_abort("dataprovider_required")
    }
    cols_to_keep <- setdiff(names(data), c("geometry"))
    data_keep <- data[, cols_to_keep] %>%
      unique()
    sb <- serve(product, data_provider, read_body_hook = read_body_hook) %>%
      mutate(geometry = .[, col_name_wkt, drop = TRUE]) %>%
      mutate(id = plz) %>%
      select(-one_of(col_name_wkt)) %>%
      st_as_sf(wkt = "geometry")
    print_debug(head(sb))
    st_crs(sb) <- 2056
    ret <- sb %>%
      inner_join(data_keep, by = "id", suffix = c("", "_y"))
    coord <- coord_sf(default = TRUE, default_crs = 2056)
    geometry_data <- ret[["geometry"]]
    geometry_crs <- st_crs(geometry_data)
    bbox <- st_bbox(geometry_data)
    coord$record_bbox(
      xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
      ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
    )
    bbox_trans <- sf_transform_xy(
      list(
        x = c(rep(0.5*(bbox[["xmin"]] + bbox[["xmax"]]), 2), bbox[["xmin"]], bbox[["xmax"]]),
        y = c(bbox[["ymin"]], bbox[["ymax"]], rep(0.5*(bbox[["ymin"]] + bbox[["ymax"]]), 2))
      ),
      coord$get_default_crs(),
      geometry_crs
    )
    ret$xmin <- min(bbox_trans$x)
    ret$xmax <- max(bbox_trans$x)
    ret$ymin <- min(bbox_trans$y)
    ret$ymax <- max(bbox_trans$y)
    ret
  },
  required_aes = c("id")
)

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
    sb <- serve("swiss_boundaries.rds", data_provider, read_body_hook = extract_swiss_boundaries) %>%
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
#' @param data_provider A datacake data provider. Default is \code{\link[datacake]{default_data_provider}}.
#' @inheritParams ggplot2::geom_polygon
#' @param gov_level Goverment level (either \dQuote{commune} or \dQuote{canton})
#'
#' @section Aesthetics:
#' The only required aesthetics is \code{id} and should contain the
#' identification for the geographical unit (canton or commune).
#' If the column is \code{numeric}, the values get interpreted as commune
#' or canton number according to the definition of the Swiss
#' Federal Statistical Office (see \url{https://www.bfs.admin.ch/bfs/de/home/grundlagen/agvch/identifikatoren-gemeinde.html}).
#' Otherwise the column gets interpreted as commune name or
#' abbreviated canton (ZH, BE, LU, \dots).
#' The aesthetics \code{x}, \code{y}, \code{group} and \code{subgroup} are ignored.
#' Otherwise the function supports the same aesthetics as \code{\link[ggplot2]{geom_polygon}}
#'
#' @examples
#' \dontrun{
#' cantons <- tibble(label = c("LU", "ZH"))
#' ggplot(cantons, aes(id = label)) +
#'   geom_canton() +
#'   geom_commune(data = tibble(label = c("Luzern", "Zürich", "Zug")), fill = "white", color = "darkgrey")
#' }
#'
#' @export
#'
geom_swiss_boundaries <- function(mapping = NULL, data = NULL, data_provider = default_data_provider(), position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, gov_level = c("canton", "commune", "plz")) {
  gov_level <- match.arg(gov_level)
  layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = gov_level, data_provider = data_provider, ...))
}

#' @rdname geom_swiss_boundaries
#' @export
geom_canton <- function(mapping = NULL, data = NULL, data_provider = default_data_provider(), position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = "canton", data_provider = data_provider, ...))

}

#' @rdname geom_swiss_boundaries
#' @export
geom_commune <- function(mapping = NULL, data = NULL, data_provider = default_data_provider(), position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = "commune", data_provider = data_provider, ...))

}

#' @rdname geom_swiss_boundaries
#' @export
geom_plz <- function(mapping = NULL, data = NULL, data_provider = default_data_provider(), position = "identity", ...,
                     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, geometry_type = c("polygon", "point")) {
  geometry_type <- match.arg(geometry_type)
  id <- if_else(geometry_type == "polygon", "geometries/plz-shape.parquet", "geometries/plz-points.parquet")
  legend <- if_else(is.na(show.legend) && geometry_type == "point", FALSE, NA)
  c(
    layer(data = data, mapping = mapping, stat = "wkt", geom = "sf",
        position = position, show.legend = legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, data_provider = data_provider, product = id, read_body_hook = extract_parquet_response, col_name_wkt = "wkt_poly", ...)),
    coord_sf(default = TRUE, default_crs = 2056)
  )
}
