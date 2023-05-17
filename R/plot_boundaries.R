
StatBoundary <- ggplot2::ggproto(
  "StatBoundary", ggplot2::Stat,
  compute_panel = function(data, scales, data_provider = NULL, gov_level = "canton") {
    cols_to_keep <- setdiff(names(data), c("x", "y", "group", "subgroup"))
    data_keep <- data[, cols_to_keep] %>%
      unique()
    sb <- data_provider$serve("swiss_boundaries", "rds") %>%
      filter(entity == gov_level)
    if (is.character(data_keep$id)) {
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

#' @export
geom_swiss_boundaries <- function(mapping = NULL, data = NULL, data_provider = NULL, position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, gov_level = c("canton", "commune")) {
  gov_level <- match.arg(gov_level, c("canton", "commune"))
  layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = gov_level, data_provider = data_provider, ...))
}

#' @export
geom_canton <- function(mapping = NULL, data = NULL, data_provider = NULL, position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = "canton", data_provider = data_provider, ...))

}

#' @export
geom_commune <- function(mapping = NULL, data = NULL, data_provider = NULL, position = "identity", ...,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = "boundary", geom = "polygon",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gov_level = "commune", data_provider = data_provider, ...))

}
