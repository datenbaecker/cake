
ask_delete_cache <- function(cache_dir) {
  cli::cli_div(theme = cli_theme())
  cake_alert("delete_cache_data", cache_data_path = cache_dir)
  repeat {
    answer <- readline() %>%
      tolower()
    if (answer %in% c("j", "n", "y")) {
      break
    }
    cake_alert("delete_cache_data", cache_data_path = cache_dir)
  }
  c("j" = TRUE, "n" = FALSE, "y" = TRUE)[answer]
}

ask_cache_data <- function() {
  cli::cli_div(theme = cli_theme())
  cake_alert_info("info_cache_data")
  cake_alert("cache_data")
  repeat {
    answer <- readline() %>%
      tolower()
    if (answer %in% c("j", "n", "y")) {
      break
    }
    cake_alert("cache_data")
  }
  c("j" = TRUE, "n" = FALSE, "y" = TRUE)[answer]
}

get_default_cache <- function() {
  rappdirs::user_cache_dir() %>%
    file.path("cake") %>%
    normalizePath(mustWork = FALSE)
}

exists_default_cache <- function() {
  cache_data_path <- get_default_cache()
  dir.exists(cache_data_path)
}

create_default_cache <- function() {
  cache_data_path <- get_default_cache()
  if (!dir.exists(cache_data_path)) {
    dir.create(cache_data_path, recursive = TRUE)
  }
  cache_data_path
}

#' Clean Data Cache
#'
#' @param data_provider An object of type \code{data_provider} (see \code{\link[cake]{datenbaecker}})
#'
#' @description Cleans the cached data in memory and on disk.
#'
#' @export
#'
clean_cache <- function(data_provider) {
  cache <- data_provider$cache
  if (inherits(cache, "FileCache")) {
    cache_dir <- cache$cache_directory()
    del_cache <- ask_delete_cache(cache_dir)
    if (del_cache) {
      cache$clean_cache()
    }
  } else {
    cache$clean_cache()
  }
}

#' @title Create Data Provider
#'
#' @param cache_dir \code{logical} if downloaded data should to be saved to disk or a \code{character} containing a directory path to the cache folder.
#' The program asks if data should be saved to disk if the argument is \code{NULL}.
#' @param auth_info Authentication info for the remote server (currently unused)
#'
#' @return An object of type \code{data_provider}
#' @export
#'
datenbaecker <- function(cache_dir = NULL, auth_info = NULL) {
  if (connection_counter() == 1L) {
    print_logo()
  }
  baecker_url <- Sys.getenv("CAKE_URL", unset = "https://datacake.datenbaecker.ch/api")
  dp <- remote_data_provider(baecker_url, cache_dir, auth_info, "v1/")
  news <- serve("news", dp, alert_download = FALSE) %>%
    resp_body_json()
  if (news != "") {
    cli_text("\n")
    cli_text(news)
  }
  invisible(dp)
}

#' @rdname datenbaecker
#' @export
local_data_provider <- function(cache_dir) {
  file_cache <- FileCache$new(cache_dir)
  list(cache = file_cache) %>%
    structure(class = c("local_data_provider", "data_provider"))
}

#' @rdname datenbaecker
#' @param host URL to the remote data provider
#' @param api_version_prefix Version prefix for the URL
#' @export
remote_data_provider <- function(host, cache_dir = NULL, auth_info = NULL, api_version_prefix = "") {
  if (is.null(cache_dir)) {
    if (exists_default_cache()) {
      cache_dir <- get_default_cache()
    } else if (ask_cache_data()) {
      cache_dir <- create_default_cache()
    }
  } else if (is.logical(cache_dir) && cache_dir) {
    cache_dir <- create_default_cache()
  }
  data_cache <- InMemoryCache$new()
  create_file_cache <- is.character(cache_dir) || (is.logical(cache_dir) && cache_dir)
  if (create_file_cache) {
    data_cache <- FileCache$new(cache_dir)
  }
  list(cache = data_cache, host = host, api_version_prefix = api_version_prefix, auth_info = auth_info) %>%
    structure(class = c("remote_data_provider", "data_provider"))
}

create_default_data_provider <- function() {
  default_provider <- NULL
  function(dp = NULL) {
    if (!is.null(dp)) {
      default_provider <<- dp
    } else if(is.null(default_provider)) {
      default_provider <<- datenbaecker()
    }
    invisible(default_provider)
  }
}

#' @rdname datenbaecker
#' @param dp Data provider that should act as default provider. Default is \code{datenbaecker()}.
#' @export
default_data_provider <- create_default_data_provider()

abort_if_wrong_class <- function(dp, expect_cls) {
  if (!inherits(dp, expect_cls)) {
    cake_abort_class(expect_cls, dp)
  }
}

extract_swiss_boundaries <- function(res) {
  file_conn <- resp_body_raw(res) %>%
    rawConnection() %>%
    gzcon()
  readRDS(file_conn)
}

extract_qs_response <- function(res) {
  obj <- resp_body_raw(res)
  file_path <- tempfile(fileext = "qs")
  writeBin(obj, file_path)
  qread(file_path)
}

extract_parquet_response <- function(res) {
  file_conn <- resp_body_raw(res) %>%
    rawConnection()
  read_parquet(file_conn)
}

download_cake <- function(dp, what, read_body_hook = identity, alert_download = TRUE) {
  abort_if_wrong_class(dp, "remote_data_provider")
  url <- file.path(dp$host, what)
  print_debug(sprintf("downloading from %s", url))
  if (alert_download) {
    cake_alert_info("downloading")
  }
  res <- request(url) %>%
    req_perform()
  read_body_hook(res)
}

set_log_level <- function(log_level="prod") {
  function(new_level = NULL) {
    if (!is.null(new_level)) {
      log_level <<- match.arg(new_level, choices = c("prod", "debug"))
    }
    log_level
  }
}

log_level <- set_log_level()

print_debug <- function(...) {
  if (log_level() == "debug") {
    print(...)
  }
}

#' Get Data from a Data Provider
#'
#' @param dp An object of type \code{data_provider} (see \code{\link[cake]{datenbaecker}})
#' @param what Name of the data source
#' @param ... Further parameter (currently not used)
#'
#' @description Depending on the type of \code{data_provider} and the settings,
#' \code{serve} might return cached data from memory or disk.
#' If the data does not exist locally, a \code{remote_data_provider} might download the
#' data from the internet and saves is to the cache folder on the disk depending
#' on the settings (see \code{\link[cake]{datenbaecker}}).
#'
#' @return The requested data
#' @export
#'
serve <- function(what, dp, ...) {
  UseMethod("serve", dp)
}

#' @export
serve.local_data_provider <- function(what, dp, ...) {
  dp$cache$get(what)
}

#' @export
serve.remote_data_provider <- function(what, dp, read_body_hook = identity, alert_download = TRUE, metadata_endpoint = NULL, ...) {
  what <- paste0(dp$api_version_prefix, what)
  dt <- dp$cache$get(what)
  if (is.null(dt)) {
    dt <- download_cake(dp, what, read_body_hook = read_body_hook, alert_download = alert_download)
    dp$cache$add(dt, what)
  }
  attr(dt, "endpoint") <- coalesce(metadata_endpoint, what)
  dt
}

order_and_serve <- function(
    what, body_json, dp, read_body_hook = identity, metadata_endpoint = NULL,
    timeout = 60
) {
  url <- file.path(dp$host, paste0(dp$api_version_prefix, what))
  print_debug(sprintf("post %s", url))
  post_res <- request(url) %>%
      req_url_query(format = "parquet") %>%
      req_body_json(body_json, auto_unbox = TRUE) %>%
      req_error(body = function(resp) resp_body_json(resp)$detail) %>%
      req_perform()
  post_res <- post_res %>%
    resp_body_json()
  on.exit(unset_cake_progress_bar_style())
  set_cake_progress_bar_style("cake")
  start_req <- Sys.time()
  prog_steps <- 10
  print_debug(post_res$taskId)
  cli_progress_bar("Processing request", total = prog_steps)
  res_ready <- FALSE
  ctr <- 0L
  while (!res_ready) {
    Sys.sleep(0.5)
    if (difftime(Sys.time(), start_req, units = "secs") > timeout) {
      cake_abort("timeout")
    }
    if (ctr < prog_steps) {
      cli_progress_update()
    }
    if (ctr %% 2 == 0) {
      curr_res <- request(url) %>%
        req_url_path_append(post_res$taskId) %>%
        req_perform() %>%
        resp_body_json()
      res_ready <- curr_res$status == "completed"
    }
    ctr <- ctr + 1
  }
  ret <- request(curr_res$result$url) %>%
    req_perform() %>%
    read_body_hook()
  attr(ret, "endpoint") <- coalesce(metadata_endpoint, what)
  ret
}

#' @title Get Labels and ID for Entities
#' @name get_cantonal_entities
#'
#' @param dp An object of type \code{data_provider} (see \code{\link[cake]{datenbaecker}})
#'
#' @return A \code{data.frame} with columns \code{id} and \code{label} containing the
#' identifiers and labels from the office of federal statistics.
#'
#' @references The source of the data is the swissBOUNDARIES3D dataset from
#' the Federal Office of Topography swisstopo:
#' \url{https://www.swisstopo.admin.ch/de/geodata/landscape/boundaries3d.html}
#'
#' @importFrom tibble remove_rownames
#' @export
#'
#' @examples
#' \dontrun{
#' # This code depends on the availability of the remote server
#' dp <- datenbaecker(cache_dir = FALSE)
#' cantons <- get_cantonal_(entities, dp = dp)
#' communes <- get_communal_entities(dp = dp)
#' }
get_cantonal_entites <- function(dp = default_data_provider()) {
  sb <- serve("geometries/cantons-shape.parquet", dp, read_body_hook = extract_parquet_response)
  sb %>%
    select(.data$bfs_num, .data$label, .data$name) %>%
    arrange(.data$bfs_num) %>%
    rename(id = .data$bfs_num)
}

#' @rdname get_cantonal_entities
#' @export
get_plz_entites <- function(dp = default_data_provider()) {
  sb <- serve("geometries/plz-shape.parquet", dp, read_body_hook = extract_parquet_response)
  sb %>%
    select(.data$plz) %>%
    unique() %>%
    mutate(id = .data$plz) %>%
    select(.data$id, .data$plz) %>%
    remove_rownames()
}

#' @rdname get_cantonal_entities
#' @export
get_communal_entites <- function(dp = default_data_provider()) {
  sb <- serve("geometries/communes-shape.parquet", dp, read_body_hook = extract_parquet_response)
  sb %>%
    select(.data$bfs_num, .data$name, .data$kanton_label) %>%
    arrange(.data$bfs_num) %>%
    rename(id = .data$bfs_num) %>%
    unique() %>%
    remove_rownames()
}
