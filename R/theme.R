
col_red <- "#b5535bff"
col_gold <- "#cc9d31f"

cli_theme <- function() {
  list(span.pkg = list(color = col_red, "font-weight" = "bold", "font-style" = "normal"))
}

print_logo <- function() {
  cli::cli_div(theme = list(
    h1 = list(
      "font-weight" = "normal",
      color = col_red,
      fmt = function(x) {
        cli::rule(x, line_col = "black")
      }
    )
  ))
  cli::cat_rule(line_col = "black")
  cli::cat_line()
  cli::cli_div(class = "tmp", theme = list(.tmp = list(color = col_red)))
  c(
    "  ██████  █████  ██   ██ ███████",
    " ██      ██   ██ ██  ██  ██     ",
    " ██      ███████ █████   █████  ",
    " ██      ██   ██ ██  ██  ██     ",
    "  ██████ ██   ██ ██   ██ ███████"
  ) %>%
    cli::cli_verbatim()
  cli::cli_h1("served by {.strong Datenbäcker GmbH}")
}

set_cake_progress_bar_style <- function(style = c("cake", "birthday_cake", "cookie", "donut", "bread", "croissant")) {
  style <- match.arg(style)
  symb <- c(
    cake = "\U1f370",
    birthday_cake = "\U1f382",
    cookie = "\U1f36a",
    donut = "\U1f369",
    bread = "\U1f35e",
    croissant = "\U1f950"
  )
  options(cli.progress_bar_style = list(
    complete = symb[style],
    incomplete = "  "
  ))
}

unset_cake_progress_bar_style <- function() {
  options(cli.progress_bar_style = NULL)
}
