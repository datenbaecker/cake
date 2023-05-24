#'
#'
#'
#' #' #' @export
#' #' datenbaecker <- function(cache_data = NULL) {
#' #'   stop("datenbaecker not working yet")
#' #'   if (is.null(cache_data)) {
#' #'     cache_data <- ask_cache_data()
#' #'   }
#' #'   if (is.logical(cache_data)) {
#' #'     if (cache_data) {
#' #'       cache_data_path <- create_default_cache()
#' #'     } else {
#' #'       cache_data_path <- NULL
#' #'     }
#' #'   } else if (is.character(cache_data)) {
#' #'     cache_data_path <- normalizePath(cache_data, mustWork = FALSE)
#' #'     if (!dir.exists(cache_data_path)) {
#' #'       datacake_abort("invalid_cache_dir", cache_data_path = cache_data_path)
#' #'     }
#' #'   } else {
#' #'     datacake_abort("invalid_cache_data_arg")
#' #'   }
#' #'   list(
#' #'     url = "https://datenbaecker.",
#' #'     cache_data = cache_data_path
#' #'   ) %>%
#' #'     structure(class = c("datenbaecker", "remote_datacake_provider", "datacake_provider"))
#' #' }
#' #'
#' #'
#' #' #' @export
#' #' local_datacake_provider <- function(resource_folder) {
#' #'   resource_folder <- normalizePath(resource_folder, mustWork = FALSE)
#' #'   if (!dir.exists(resource_folder)) {
#' #'     datacake_abort("invalid_cache_dir", cache_data_path = resource_folder)
#' #'   }
#' #'   list(
#' #'     resource_folder = resource_folder
#' #'   ) %>%
#' #'     structure(class = c("local_datacake_provider", "datacake_provider"))
#' #' }
#'
#'
#' menu_to_cache <- function(menu) {
#'   lapply(menu, function(flavor_names) {
#'     flavors <- vector(mode = "list", length = length(flavor_names))
#'     names(flavors) <- flavor_names
#'     flavors
#'   })
#' }
#'
#' create_import_function <- function(base_path, ext, read_func) {
#'   function(what) {
#'     file_path <- file.path(base_path, paste0(what, ext))
#'     read_func(file_path)
#'   }
#' }
#'
#'
#' create_export_function <- function(base_path, ext, write_func) {
#'   function(data, what) {
#'     file_path <- file.path(base_path, paste0(what, ext))
#'     write_func(data, file_path)
#'   }
#' }


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

local_data_provider <- function(cache_dir) {
  file_cache <- FileCache$new(cache_dir)
  list(cache = file_cache) %>%
    structure(class = c("local_data_provider", "data_provider"))
}

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

create_default_cache <- function() {
  cache_data_path <- rappdirs::user_cache_dir() %>%
    file.path("datacake") %>%
    normalizePath(mustWork = FALSE)
  if (!dir.exists(cache_data_path)) {
    dir.create(cache_data_path, recursive = TRUE)
  }
  cache_data_path
}

remote_data_provider <- function(host, cache_dir = NULL) {
  if (is.null(cache_dir) && ask_cache_data()) {
    cache_dir <- create_default_cache()
  }
  data_cache <- InMemoryCache$new()
  if (!is.null(cache_dir)) {
    data_cache <- FileCache$new(cache_dir)
  }
  list(cache = data_cache, host = host) %>%
    structure(class = c("remote_data_provider", "data_provider"))
}

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

# # dp <- remote_data_provider("localhost")
# dp_local <- local_data_provider("path/to/folder")
# serve("swiss_boundaries", dp_local)

# serve("swiss_boundaries", dp_local) %>%
#   filter(entity == "canton") %>%
#   ggplot(aes(id = label)) +
#   geom_canton(data_provider = dp_local)

#'
#' InMemoryDatacakeProviderClass <- R6::R6Class(
#'   "InMemoryDatacakeProvider",
#'   public = list(
#'     initialize = function(menu = list()) {
#'       private$cached_data <- menu_to_cache(menu)
#'     }
#'   ),
#'   private = list(
#'     cached_data = list(),
#'     get_cached_data = function(name) {
#'       dt <- NULL
#'       if (name %in% names(private$cached_data)) {
#'         dt <- private$cached_data[[name]]
#'       }
#'       dt
#'     },
#'     add_data = function(data, name) {
#'       private$cached_data[[name]] <- data
#'     }
#'   )
#' )
#'
#'
#' LocalDatacakeProviderClass <- R6::R6Class(
#'   "LocalDatacakeProvider",
#'   inherit = InMemoryDatacakeProviderClass,
#'   public = list(
#'     initialize = function(resource_folder) {
#'       resource_folder <- normalizePath(resource_folder, mustWork = FALSE)
#'       if (!dir.exists(resource_folder)) {
#'         datacake_abort("invalid_cache_dir", cache_data_path = resource_folder)
#'       }
#'       self$resource_folder <- resource_folder
#'       # private$cached_data <- menu_to_cache(self$menu())
#'       super$initialize(self$menu())
#'       private$read_fct <- list(
#'         rds = create_import_function(self$resource_folder, ".rds", readRDS)
#'       )
#'       private$write_fct <- list(
#'         rds = create_export_function(self$resource_folder, ".rds", saveRDS)
#'       )
#'     },
#'     serve = function(what, flavor) {
#'       if (!self$exists(what, flavor)) {
#'         datacake_abort("not_in_menu")
#'       }
#'       # dt <- super$get_cached_data(what, flavor)
#'       # if (is.null(dt)) {
#'       #   import <- private$read_fct[[flavor]]
#'       #   dt <- import(what)
#'       #   private$add_data(dt, what, flavor)
#'       # }
#'       dt <- self$get_cached_data(what, flavor)
#'       dt
#'     },
#'     menu = function() {
#'       all_files <- list.files(self$resource_folder)
#'       basenames <- tools::file_path_sans_ext(all_files)
#'       ret <- unique(basenames) %>%
#'         lapply(function(x) tools::file_ext(all_files[basenames == x]))
#'       names(ret) <- unique(basenames)
#'       ret
#'     },
#'     exists = function(what, flavor) {
#'       menu <- self$menu()
#'       what %in% names(menu) && flavor %in% menu[[what]]
#'     },
#'     resource_folder = NULL
#'   ),
#'   private = list(
#'     read_fct = NULL,
#'     write_fct = NULL,
#'     get_cached_data = function(what, flavor) {
#'       data <- super$get_cached_data(what, flavor)
#'       if (is.null(data) && self$exists(what, flavor)) {
#'         import <- private$read_fct[[flavor]]
#'         data <- import(what)
#'         super$add_data(data, what, flavor)
#'       }
#'       data
#'     }
#'   )
#' )
#'
#' #' @export
#' LocalDatacakeProvider <- function(resource_path) {
#'   LocalDatacakeProviderClass$new(resource_path)
#' }
#'
#' #' download_rds <- function(what) {
#' #'   url <- file.path(host, paste0(what, ".rds"))
#' #'   res <- httr::GET(url)
#' #'   file_conn <- httr::content(res) %>%
#' #'     rawConnection() %>%
#' #'     gzcon()
#' #'   readRDS(file_conn)
#' #' }
#' #'
#' #' ask_cache_data <- function() {
#' #'   cli::cli_div(theme = cli_theme())
#' #'   datacake_alert_info("info_cache_data")
#' #'   datacake_alert("cache_data")
#' #'   repeat {
#' #'     answer <- readline() %>%
#' #'       tolower()
#' #'     if (answer %in% c("j", "n", "y")) {
#' #'       break
#' #'     }
#' #'     datacake_alert("cache_data")
#' #'   }
#' #'   c("j" = TRUE, "n" = FALSE, "y" = TRUE)[answer]
#' #' }
#' #'
#' #' create_default_cache <- function() {
#' #'   cache_data_path <- rappdirs::user_cache_dir() %>%
#' #'     file.path("datacake") %>%
#' #'     normalizePath(mustWork = FALSE)
#' #'   if (!dir.exists(cache_data_path)) {
#' #'     dir.create(cache_data_path, recursive = TRUE)
#' #'   }
#' #'   cache_data_path
#' #' }
#' #'
#' #' RemoteDatacakeProviderClass <- R6::R6Class(
#' #'   "RemoteDatacakeProvider",
#' #'   public = list(
#' #'     initialize = function(host, cache_data = NULL) {
#' #'       self$host <- host
#' #'       private$data_catalog <- paste0(host, "/data-catalog") %>%
#' #'         httr::GET() %>%
#' #'         httr::content(as = "parsed")
#' #'       private$cached_data <- menu_to_cache(self$menu())
#' #'       private$read_fct <- list(
#' #'         rds = download_rds
#' #'       )
#' #'       if (is.null(cache_data) && ask_cache_data()) {
#' #'         cache_data <- create_default_cache()
#' #'       }
#' #'       if (!is.null(cache_data)) {
#' #'
#' #'       }
#' #'     },
#' #'     serve = function(what, flavor) {
#' #'       if (!self$exists(what, flavor)) {
#' #'         datacake_abort("not_in_menu")
#' #'       }
#' #'       dt <- private$get_cached_data(what, flavor)
#' #'       if (is.null(dt)) {
#' #'         import <- private$read_fct[[flavor]]
#' #'         dt <- import(what)
#' #'         private$cached_data[[what]][[flavor]] <- dt
#' #'       }
#' #'       dt
#' #'     },
#' #'     menu = function() {
#' #'       self$data_catalog
#' #'     },
#' #'     exists = function(what, flavor) {
#' #'       menu <- self$menu()
#' #'       what %in% names(menu) && flavor %in% menu[[what]]
#' #'     },
#' #'     host = NULL
#' #'   ),
#' #'   private = list(
#' #'     cached_data = list(),
#' #'     get_cached_data = function(what, flavor) {
#' #'       dt <- NULL
#' #'       if (what %in% names(private$cached_data) && flavor %in% names(private$cached_data[[what]])) {
#' #'         dt <- private$cached_data[[what]][[flavor]]
#' #'       }
#' #'       dt
#' #'     },
#' #'     read_fct = NULL,
#' #'     data_catalog = NULL,
#' #'   )
#' #' )
#' #'
#' #' #' @export
#' #' RemoteDatacakeProvider <- function(host) {
#' #'   RemoteDatacakeProviderClass$new(host)
#' #' }
