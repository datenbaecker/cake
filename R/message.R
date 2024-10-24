
supported_lang <- function() {
  c("en", "de", "fr", "it")
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

get_cake_msg <- function(what) {
  lang <- get_lang()
  msg <- list(
    delete_cache_data = c(
      "en" = "{.strong This command deletes all data in {.path {cache_data_path}}. Do you wish to continue (y/n)?}",
      "de" = "{.strong Dieser Befehl l\u00f6scht s\u00e4mtliche Daten in {.path {cache_data_path}}. Willst du fortfahren (j/n)?}"
    ),
    cache_data = c(
      "en" = "{.strong Would you like to save the data on your disc? (y/n)}",
      "de" = "{.strong M\u00f6chtest du die Daten lokal speichern? (j/n)}"
    ),
    info_cache_data = c(
      "en" = "{.pkg cake} may save the data on your local disc.
        This speeds up data loading in subsequent R session.
        Specify {.arg cache_data} if you don't want to answer this question in future calls.",
       "de" = "{.pkg cake} kann die Daten auf deiner lokalen Festplatte speichern.
        Das beschleunigt das Laden der Daten in zuk\u00fcnftigen R-Sessions.
        Spezifiziere {.arg cache_data} falls du diese Frage zuk\u00fcnftig nicht mehr erhalten m\u00f6chtest."
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
      "de" = "Wert f\u00fcr {.arg what} oder {.arg flavor} ist nicht g\u00fcltig. Siehe {.code menu} f\u00fcr Datenkatalog."
    ),
    dataprovider_required = c(
      "en" = "No value for {.arg data_provider} provided."
    ),
    abort_class = c(
      "en" = "Expected object of type {.cls {expected}} for argument {.arg {argument}}"
    ),
    no_endpoint = c(
      "en" = "Attribute {.code endpoint} not found. Did you already edit the object?"
    ),
    no_metadata = c(
      "en" = "No metadata available for this object."
    ),
    timeout = c(
      "en" = "The server did not respond in the usual amount of time. Please try again later..."
    )
  )
  rel_msg <- msg[[what]]
  if (!lang %in% names(rel_msg)) {
    lang <- "en"
  }
  rel_msg[lang]
}

cake_abort <- function(err, ...) {
  with(list(...), {
    get_cake_msg(err) %>%
      cli::cli_abort()
  })
}

cake_abort_class <- function(expected, arg) {
  cake_abort("abort_class", expected = expected, argument = deparse(substitute(arg)))
}

cake_alert <- function(msg, ...) {
  with(list(...), {
    get_cake_msg(msg) %>%
      cli::cli_alert()
  })
}

cake_alert_info <- function(msg, ...) {
  with(list(...), {
    get_cake_msg(msg) %>%
      cli::cli_alert_info()
  })
}

