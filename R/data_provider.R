
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

#' #' @export
#' datenbaecker <- function(cache_data = NULL) {
#'   stop("datenbaecker not working yet")
#'   if (is.null(cache_data)) {
#'     cache_data <- ask_cache_data()
#'   }
#'   if (is.logical(cache_data)) {
#'     if (cache_data) {
#'       cache_data_path <- create_default_cache()
#'     } else {
#'       cache_data_path <- NULL
#'     }
#'   } else if (is.character(cache_data)) {
#'     cache_data_path <- normalizePath(cache_data, mustWork = FALSE)
#'     if (!dir.exists(cache_data_path)) {
#'       datacake_abort("invalid_cache_dir", cache_data_path = cache_data_path)
#'     }
#'   } else {
#'     datacake_abort("invalid_cache_data_arg")
#'   }
#'   list(
#'     url = "https://datenbaecker.",
#'     cache_data = cache_data_path
#'   ) %>%
#'     structure(class = c("datenbaecker", "remote_datacake_provider", "datacake_provider"))
#' }
#'
#'
#' #' @export
#' local_datacake_provider <- function(resource_folder) {
#'   resource_folder <- normalizePath(resource_folder, mustWork = FALSE)
#'   if (!dir.exists(resource_folder)) {
#'     datacake_abort("invalid_cache_dir", cache_data_path = resource_folder)
#'   }
#'   list(
#'     resource_folder = resource_folder
#'   ) %>%
#'     structure(class = c("local_datacake_provider", "datacake_provider"))
#' }


menu_to_cache <- function(menu) {
  lapply(menu, function(flavor_names) {
    flavors <- vector(mode = "list", length = length(flavor_names))
    names(flavors) <- flavor_names
    flavors
  })
}

create_import_function <- function(base_path, ext, read_func) {
  function(what) {
    file_path <- file.path(base_path, paste0(what, ext))
    print(file_path)
    read_func(file_path)
  }
}


# DataProviderClass <- R6::R6Class(
#   "DataProvider",
#   public = list(
#     serve =
#   )
# )

LocalDatacakeProviderClass <- R6::R6Class(
  "LocalDatacakeProvider",
  public = list(
    initialize = function(resource_folder) {
      resource_folder <- normalizePath(resource_folder, mustWork = FALSE)
      if (!dir.exists(resource_folder)) {
        datacake_abort("invalid_cache_dir", cache_data_path = resource_folder)
      }
      self$resource_folder <- resource_folder
      private$cached_data <- menu_to_cache(self$menu())
      private$read_fct <- list(
        rds = create_import_function(self$resource_folder, ".rds", readRDS)
      )
    },
    serve = function(what, flavor) {
      if (!self$exists(what, flavor)) {
        datacake_abort("not_in_menu")
      }
      dt <- private$get_cached_data(what, flavor)
      if (is.null(dt)) {
        import <- private$read_fct[[flavor]]
        dt <- import(what)
        private$cached_data[[what]][[flavor]] <- dt
      }
      dt
    },
    menu = function() {
      all_files <- list.files(self$resource_folder)
      basenames <- tools::file_path_sans_ext(all_files)
      ret <- unique(basenames) %>%
        lapply(function(x) tools::file_ext(all_files[basenames == x]))
      names(ret) <- unique(basenames)
      ret
    },
    exists = function(what, flavor) {
      menu <- self$menu()
      what %in% names(menu) && flavor %in% menu[[what]]
    },
    resource_folder = NULL
  ),
  private = list(
    cached_data = list(),
    get_cached_data = function(what, flavor) {
      dt <- NULL
      if (what %in% names(private$cached_data) && flavor %in% names(private$cached_data[[what]])) {
        dt <- private$cached_data[[what]][[flavor]]
      }
      dt
    },
    read_fct = NULL
  )
)

#' @export
LocalDatacakeProvider <- function(resource_path) {
  LocalDatacakeProviderClass$new(resource_path)
}

#' download_rds <- function(what) {
#'   url <- file.path(host, paste0(what, ".rds"))
#'   res <- httr::GET(url)
#'   file_conn <- httr::content(res) %>%
#'     rawConnection() %>%
#'     gzcon()
#'   readRDS(file_conn)
#' }
#'
#' RemoteDatacakeProviderClass <- R6::R6Class(
#'   "RemoteDatacakeProvider",
#'   public = list(
#'     initialize = function(host) {
#'       self$host <- host
#'       private$data_catalog <- paste0(host, "/data-catalog") %>%
#'         httr::GET() %>%
#'         httr::content(as = "parsed")
#'       private$cached_data <- menu_to_cache(self$menu())
#'       private$read_fct <- list(
#'         rds = download_rds
#'       )
#'     },
#'     serve = function(what, flavor) {
#'       if (!self$exists(what, flavor)) {
#'         datacake_abort("not_in_menu")
#'       }
#'       dt <- private$get_cached_data(what, flavor)
#'       if (is.null(dt)) {
#'         import <- private$read_fct[[flavor]]
#'         dt <- import(what)
#'         private$cached_data[[what]][[flavor]] <- dt
#'       }
#'       dt
#'     },
#'     menu = function() {
#'       self$data_catalog
#'     },
#'     exists = function(what, flavor) {
#'       menu <- self$menu()
#'       what %in% names(menu) && flavor %in% menu[[what]]
#'     },
#'     host = NULL
#'   ),
#'   private = list(
#'     cached_data = list(),
#'     get_cached_data = function(what, flavor) {
#'       dt <- NULL
#'       if (what %in% names(private$cached_data) && flavor %in% names(private$cached_data[[what]])) {
#'         dt <- private$cached_data[[what]][[flavor]]
#'       }
#'       dt
#'     },
#'     read_fct = NULL,
#'     data_catalog = NULL
#'   )
#' )
#'
#' #' @export
#' LocalDatacakeProvider <- function(resource_path) {
#'   LocalDatacakeProviderClass$new(resource_path)
#' }
