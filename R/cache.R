
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
      file.path(private$cache_dir, paste0(name, ".qs"))
    },
    load_file = function(name) {
      file_path <- private$get_filepath(name)
      qread(file_path)
    },
    save_file = function(obj, name) {
      file_path <- private$get_filepath(name)
      dir_name <- dirname(file_path)
      if (!dir.exists(dir_name)) {
        dir.create(dir_name, recursive = TRUE)
      }
      qsave(obj, file_path)
    }
  )
)
