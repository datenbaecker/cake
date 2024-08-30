
create_counter <- function(init = 0) {
  val <- init
  function(inc = 1L) {
    val <<- val + inc
    val
  }
}

connection_counter <- create_counter()
