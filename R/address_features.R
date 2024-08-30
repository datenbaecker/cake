
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

#' @export
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
      filter(name %in% colnames(x))
  }
  class(mdt) <- c("cake_metadata", class(mdt))
  show(mdt, ...)
  invisible(mdt)
}

#' @export
print.cake_metadata <- function(metadata, lang = get_lang(), include_description = TRUE) {
  source_col <- paste0("source_", lang)
  for (src in unique(metadata[, source_col, drop = TRUE])) {
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
      filter(.[, source_col, drop = TRUE] == src) %>%
      arrange(name)
    li <- cli_ul()
    for (col in curr_cols$name) {
      cli_li(style_bold(col))
      if (include_description) {
        col_desc <- metadata %>%
          filter(name == col) %>%
          select(en, de, fr, it) %>%
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
