
InMemoryCache <- R6::R6Class(
  "InMemoryCache",
  public = list(
    initialize = function() {
      cache <- vector(mode = "list", length = 1000L)
      names(cache) <- rep("", 1000L)
      private$cached_data <- cache
    },
    get = function(name) {
      dt <- NULL
      if (self$exists(name)) {
        dt <- private$cached_data[[name]]
      }
      dt
    },
    exists = function(name) {
      self$exists_in_memory(name)
    },
    exists_in_memory = function(name) {
      name %in% names(private$cached_data)
    },
    add = function(obj, name, overwrite = FALSE) {
      message("adding to cache")
      if (!self$exists_in_memory(name) || overwrite) {
        message("add...")
        idx <- private$get_empty_slot()
        if (idx > length(private$cached_data)) {
          stop("max data cache size reached")
        }
        private$cached_data[[idx]] <- obj
        names(private$cached_data)[idx] <- name
      }
    }
  ),
  private = list(
    cached_data = NULL,
    get_empty_slot = function() {
      min(which(names(private$cached_data) == ""))
    }
  )
)


FileCache <- R6::R6Class(
  "FileCache",
  inherit = InMemoryCache,
  public = list(
    initialize = function(cache_dir) {
      super$initialize()
      private$cache_dir <- path.expand(cache_dir) %>%
        normalizePath(mustWork = FALSE)
      if (!dir.exists(private$cache_dir)) {
        dir.create(private$cache_dir)
      }
    },
    get = function(name) {
      dt <- super$get(name)
      if (is.null(dt) && self$exists(name)) {
        message("loading file")
        dt <- private$load_file(name)
        super$add(dt, name)
      }
      dt
    },
    exists = function(name) {
      file_path <- private$get_filepath(name)
      super$exists(name) || file.exists(file_path)
    },
    add = function(obj, name, overwrite = FALSE) {
      if (!self$exists(name) || overwrite) {
        super$add(obj, name, overwrite)
        private$save_file(obj, name)
      }
    }
  ),
  private = list(
    cache_dir = NULL,
    get_filepath = function(name) {
      file.path(private$cache_dir, paste0(name, ".rds"))
    },
    load_file = function(name) {
      file_path <- private$get_filepath(name)
      readRDS(file_path)
    },
    save_file = function(obj, name) {
      file_path <- private$get_filepath(name)
      saveRDS(obj, file_path)
    }
  )
)

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
#' @param cache_dir
#'
#' @return
#' @export
#'
#' @examples
datenbaecker <- function(cache_dir = NULL) {
  remote_data_provider("localhost:3890", cache_dir)
}

#' @rdname datenbaecker
#' @export
local_data_provider <- function(cache_dir) {
  file_cache <- FileCache$new(cache_dir)
  list(cache = file_cache) %>%
    structure(class = c("local_data_provider", "data_provider"))
}

#' @rdname datenbaecker
#' @param host
#' @export
remote_data_provider <- function(host, cache_dir = NULL) {
  if (is.null(cache_dir)) {
    if (exists_default_cache()) {
      cache_dir <- get_default_cache()
    } else if (ask_cache_data()) {
      cache_dir <- create_default_cache()
    }
  }
  data_cache <- InMemoryCache$new()
  if (!is.null(cache_dir)) {
    data_cache <- FileCache$new(cache_dir)
  }
  list(cache = data_cache, host = host) %>%
    structure(class = c("remote_data_provider", "data_provider"))
}

create_default_data_provider <- function() {
  default_provider <- datenbaecker()
  function(dp = NULL) {
    if (!is.null(dp)) {
      default_provider <<- dp
    }
    default_provider
  }
}

#' @rdname datenbaecker
#' @param dp Data provider that should act as default provider. Default is \code{datenbaecker()}.
#' @export
default_data_provider <- create_default_data_provider()

download_datacake <- function(dp, what) {
  expect_cls <- "remote_data_provider"
  if (!inherits(dp, expect_cls)) {
    datacake_abort_class(expect_cls, dp)
  }
  url <- file.path(dp$host, paste0(what, ".rds"))
  res <- httr::GET(url)
  file_conn <- httr::content(res) %>%
    rawConnection() %>%
    gzcon()
  readRDS(file_conn)
}

#' Get data
#'
#' @param dp
#' @param what
#' @param ...
#'
#' @return The requested data
#' @export
#'
serve <- function(what, dp, ...) {
  UseMethod("serve", dp)
}

#' @export
serve.local_data_provider <- function(what, dp) {
  dp$cache$get(what)
}

#' @export
serve.remote_data_provider <- function(what, dp) {
  dt <- dp$cache$get(what)
  if (is.null(dt)) {
    dt <- download_datacake(dp, what)
    dp$cache$add(dt, what)
  }
  dt
}

menu <- function() {
  UseMethod("menu", dp)
}

ask <- function(dp, about, considering = c("license")) {

}

# db <- datenbaecker()
# dp_local <- local_data_provider("path/to/folder")
# swiss_boundaries <- serve("swiss_boundaries", db)

# serve("swiss_boundaries", dp_local) %>%
#   filter(entity == "canton") %>%
#   ggplot(aes(id = label)) +
#   geom_canton(data_provider = dp_local)
