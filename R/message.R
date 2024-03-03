
supported_lang <- function() {
  c("en", "de", "fr")
}

get_lang <- function() {
  lang <- "en"
  lang_env <- Sys.getenv("LANG") %>%
    substr(1, 2)
  if (lang_env %in% supported_lang()) {
    lang <- lang_env
  }
  lang
}

get_datacake_msg <- function(what) {
  lang <- get_lang()
  msg <- list(
    cache_data = c(
      "en" = "{.strong Would you like to save the data on your disc? (y/n)}",
      "de" = "{.strong Möchtest du die Daten lokal speichern? (j/n)}"
    ),
    info_cache_data = c(
      "en" = "{.pkg datacake} may save the data on your local disc.
        This speeds up data loading in subsequent R session.
        Specify {.arg cache_data} if you don't want to answer this question in future calls.",
       "de" = "{.pkg datacake} kann die Daten auf deiner lokalen Festplatte speichern.
        Das beschleunigt das Laden der Daten in zukünftigen R-Sessions.
        Spezifiziere {.arg cache_data} falls du diese Frage zukünftig nicht mehr erhalten möchtest."
    ),
    invalid_cache_data_arg = c(
      "en" = "{.arg cache_data} must be of class {.cls character} or {.cls logical}.",
      "de" = "{.arg cache_data} muss vom Typ {.cls character} oder {.cls logical} sein."
    ),
    invalid_cache_dir = c(
      "en" = "Directory {.path {cache_data_path}} does not exist.",
      "de" = "Das Verzeichnis {.path {cache_data_path}} existiert nicht."
    ),
    downloading = c(
      "en" = "Downloading data, this takes a while...",
      "de" = "Daten werden heruntergeladen. Dieser Vorgang dauert eine Weile..."
    ),
    not_in_menu = c(
      "en" = "Value for {.arg what} or {.arg flavor} is invalid. Check {.code menu} for data catalog.",
      "de" = "Wert für {.arg what} oder {.arg flavor} ist nicht gültig. Siehe {.code menu} für Datenkatalog."
    ),
    dataprovider_required = c(
      "en" = "No value for {.arg data_provider} provided."
    ),
    abort_class = c(
      "en" = "Expected object of type {.cls {expected}} for argument {.arg {argument}}"
    )
  )
  rel_msg <- msg[[what]]
  if (!lang %in% names(rel_msg)) {
    lang <- "en"
  }
  rel_msg[lang]
}

datacake_abort <- function(err, ...) {
  with(list(...), {
    get_datacake_msg(err) %>%
      cli::cli_abort()
  })
}

datacake_abort_class <- function(expected, arg) {
  datacake_abort("abort_class", expected = expected, argument = deparse(substitute(arg)))
}

datacake_alert <- function(msg, ...) {
  with(list(...), {
    get_datacake_msg(msg) %>%
      cli::cli_alert()
  })
}

datacake_alert_info <- function(msg, ...) {
  with(list(...), {
    get_datacake_msg(msg) %>%
      cli::cli_alert_info()
  })
}

