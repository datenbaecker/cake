
ask_cache_data <- function() {
  cli::cli_div(theme = cli_theme())
  datacake_alert_info("info_cache_data")
  datacake_alert("cache_data")
  repeat {
    answer <- readline() %>%
      tolower()
    if (answer %in% c("j", "n", "y")) {
      break
    }
    datacake_alert("cache_data")
  }
  c("j" = TRUE, "n" = FALSE, "y" = TRUE)[answer]
}

get_default_cache <- function() {
  rappdirs::user_cache_dir() %>%
    file.path("datacake") %>%
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

#' @title Create Data Provider
#'
#' @param cache_dir \code{logical} if downloaded data should to be saved to disk or a \code{character} containing a directory path to the cache folder.
#' The program asks if data should be saved to disk if the argument is \code{NULL}.
#'
#' @return An object of type \code{data_provider}
#' @export
#'
datenbaecker <- function(cache_dir = NULL, auth_info = NULL) {
  print_logo()
  baecker_url <- Sys.getenv("DATACAKE_URL", unset = "https://datacake.datenbaecker.ch")
  dp <- remote_data_provider(baecker_url, cache_dir, auth_info)
  dp
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
#' @export
remote_data_provider <- function(host, cache_dir = NULL, auth_info = NULL) {
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
  list(cache = data_cache, host = host, auth_info = auth_info) %>%
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
    default_provider
  }
}

#' @rdname datenbaecker
#' @param dp Data provider that should act as default provider. Default is \code{datenbaecker()}.
#' @export
default_data_provider <- create_default_data_provider()

abort_if_wrong_class <- function(dp, expect_cls) {
  if (!inherits(dp, expect_cls)) {
    datacake_abort_class(expect_cls, dp)
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

download_datacake <- function(dp, what, read_body_hook = identity) {
  abort_if_wrong_class(dp, "remote_data_provider")
  url <- file.path(dp$host, what)
  datacake_alert_info("downloading")
  res <- request(url) %>%
    req_perform()
  read_body_hook(res)
}

# order_datacake <- function(dp, what, read_body_hook = identity, ...) {
#   abort_if_wrong_class(dp, "remote_data_provider")
#   # res <- request(url) %>%
#
#
# }

#' Get Data from a Data Provider
#'
#' @param dp An object of type \code{data_provider} (see \code{\link[datacake]{datenbaecker}})
#' @param what Name of the data source
#' @param ... Further parameter (currently not used)
#'
#' @description Depending on the type of \code{data_provider} and the settings,
#' \code{serve} might return cached data from memory or disk.
#' If the data does not exist locally, a \code{remote_data_provider} might download the
#' data from the internet and saves is to the cache folder on the disk depending
#' on the settings (see \code{\link[datacake]{datenbaecker}}).
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
serve.remote_data_provider <- function(what, dp, read_body_hook = identity) {
  dt <- dp$cache$get(what)
  if (is.null(dt)) {
    dt <- download_datacake(dp, what, read_body_hook = read_body_hook)
    dp$cache$add(dt, what)
  }
  dt
}


#' @title Get Labels and ID for Entities
#'
#' @param dp An object of type \code{data_provider} (see \code{\link[datacake]{datenbaecker}})
#'
#' @return A \code{data.frame} with columns \code{id} and \code{label} containing the
#' identifiers and labels from the office of federal statistics.
#' \code{get_statistical_units} returns all entities and has an additional column
#' \code{entity} to identify the type of entity.
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
#' dp <- datenbaecker()
#' cantons <- get_cantonal(entities)
#' communes <- get_communal_entities()
#' }
get_statistical_entities <- function(dp) {
  ct <- get_cantonal_entites(dp) %>%
    mutate(entity = "canton")
  get_communal_entities(dp) %>%
    mutate(entity = "commune")
  bind_rows(ct, cm) %>%
    remove_rownames()
}

#' @rdname get_statistical_entities
#' @export
get_cantonal_entites <- function(dp) {
  sb <- serve("swiss_boundaries.rds", dp, read_body_hook = extract_swiss_boundaries)
  sb %>%
    filter(entity == "canton") %>%
    select(bfs_num, label) %>%
    arrange(bfs_num) %>%
    rename(id = bfs_num) %>%
    unique() %>%
    remove_rownames()
}

get_plz_entites <- function(dp) {
  sb <- serve("")
}

#' @rdname get_statistical_entities
#' @export
get_communal_entites <- function(dp) {
  sb <- serve("swiss_boundaries.rds", dp, read_body_hook = extract_swiss_boundaries)
  sb %>%
    filter(entity == "commune") %>%
    select(bfs_num, label) %>%
    arrange(bfs_num) %>%
    rename(id = bfs_num) %>%
    unique() %>%
    remove_rownames()
}

# menu <- function() {
#   UseMethod("menu", dp)
# }
#
# ask <- function(dp, about, considering = c("license")) {
#
# }
