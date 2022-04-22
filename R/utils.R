#' gets characters (line) from the terminal or from a connection
#'
#' @return string

getLine <- function() {
  if (interactive()) {
    s <- readline()
  } else {
    con <- file("stdin")
    defer({
      close(con)
    })
    s <- readLines(con, 1, warn = FALSE)
  }
  return(s)
}


# use left_join, right_join, full_join and inner_join without messages such as 'joined by "all_regi"'
left_join_silent <- function(...) {
  return(suppressMessages(left_join(...)))
}
right_join_silent <- function(...) {
  return(suppressMessages(right_join(...)))
}
full_join_silent <- function(...) {
  return(suppressMessages(full_join(...)))
}
inner_join_silent <- function(...) {
  return(suppressMessages(inner_join(...)))
}
