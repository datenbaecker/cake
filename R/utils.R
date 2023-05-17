
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
    "██████   █████  ████████  █████   ██████  █████  ██   ██ ███████",
    "██   ██ ██   ██    ██    ██   ██ ██      ██   ██ ██  ██  ██ ",
    "██   ██ ███████    ██    ███████ ██      ███████ █████   █████ ",
    "██   ██ ██   ██    ██    ██   ██ ██      ██   ██ ██  ██  ██",
    "██████  ██   ██    ██    ██   ██  ██████ ██   ██ ██   ██ ███████"
  ) %>%
    cli::cli_verbatim()
  # cli::cat_line()
  cli::cli_h1("served by {.strong Datenbäcker GmbH}")
}


# cat_bakery_rule <- function(x = "") {
#   total_width <- cli::console_width() / 8
#   x_len <- nchar(x)
#   left_len <- 1
#   right_len <- total_width - x_len - left_len
#   bakeries <- c("🍞","🍰" ,"🥐", "🍪")
#   left_rule <- rep(bakeries, left_len) %>%
#     paste(collapse = "")
#   right_rule <- rep(bakeries, right_len) %>%
#     paste(collapse = "")
#   paste0(left_rule, x, right_rule, collapse = " ") %>%
#     cat()
# }



