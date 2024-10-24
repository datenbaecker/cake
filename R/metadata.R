
#' Get Metadata for an Object
#'
#' @param x Object you want to get metadata for
#' @param data_provider An object of type \code{data_provider} (see \code{\link[cake]{datenbaecker}})
#' @param show A method to print the metadata (default is \code{print})
#' @param ... Further arguments that get passed to the \code{show} method
#'
#' @return An invisible data frame with the metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # This code depends on the availability of the remote server
#' dp <- datenbaecker(cache_dir = FALSE)
#' adr_feat <- get_address_features(data_provider = dp)
#' metadata(adr_feat[, c("com_name", "com_canton", "bev_total")])
#' }
metadata <- function(x, data_provider = default_data_provider(), show = print, ...) {
  what <- attr(x, "endpoint")
  if (is.null(what)) {
    cake_abort("no_endpoint")
  }
  mdt <- try(cake::serve(paste0(what, "/metadata"), data_provider, read_body_hook = extract_parquet_response))
  if (inherits(mdt, "try-error")) {
    message("")
  }
  if (inherits(x, "data.frame")) {
    mdt <- mdt %>%
      filter(.data$name %in% colnames(x))
  }
  class(mdt) <- c("cake_metadata", class(mdt))
  show(mdt, ...)
  invisible(mdt)
}

#' @export
print.cake_metadata <- function(x, lang = get_lang(), include_description = TRUE, ...) {
  metadata <- x
  source_col <- paste0("source_", lang)
  all_sources <- unique(metadata[, source_col, drop = TRUE]) %>%
    sort()
  for (src in all_sources) {
    d <- cli::cli_div(
      theme = list(
        h2 = list(
          color = col_red,
          fmt = function(x) {
            cli::rule(x, line_col = "black")
          }
        )
      )
    )
    cli_h2(src)
    cli::cli_end()
    curr_cols <- metadata %>%
      filter(.data[[source_col]] == src) %>%
      arrange(.data$name)
    li <- cli_ul()
    for (col in curr_cols$name) {
      cli_li(style_bold(col))
      if (include_description) {
        col_desc <- metadata %>%
          filter(.data$name == col) %>%
          select(.data$en, .data$de, .data$fr, .data$it) %>%
          as.list()
        col_desc <- col_desc[c(lang, setdiff(supported_lang(), lang))]
        col_desc <- col_desc[sapply(col_desc, function(x) !is.na(x))]
        if (length(col_desc)) {
          d <- cli_div(class = "coldesc", theme = list(.coldesc = list(before = "  ")))
          cli_text(col_desc[[1]])
          cli_end(d)
        }
      }
    }
    cli_end(li)
  }
}
