#' Variable Names as Hierarchical List
#'
#' Take a character vector of variables names structured into groups and
#' subgroups by \code{|} in the name and convert it into a hierarchical
#' structure of named lists.
#'
#' @param vars A character vector of variable names. Hierarchical section
#'   structure of variables is declared by the symbol \code{|}.
#' @param entry A string determining the entries of all leafs and the nodes
#'   which represent variables in \code{vars}. \code{"NULL"} puts \code{NULL}
#'   into the leafs of the resulting list. \code{"name"} places the full name of
#'   an existing variable in the respective node. \code{"count"} shows the
#'   number of occurrences of the respective variable name in \code{vars} as the
#'   first entry of a node.
#' @return A hierarchical named list with the structure defined by the use of
#'   \code{|}.
#' @author Christof Schoetz
#' @examples
#' vars <- c(
#'   "Emi|GHG|CO2", "Emi|GHG|CH4", "Emi|NOX",
#'   "FE", "FE|Buildings", "FE|Industry", "FE|Transport")
#' varList <- variablesAsList(vars)
#' varList <- variablesAsList(vars, "count")
#' \dontrun{View(varList)}
variablesAsList <- function(vars, entry = c("NULL", "name", "count")) {
  entry <- match.arg(entry)
  vars <- as.character(vars)
  varsTable <- table(vars)
  uniqueVars <- names(varsTable)
  splitList <- strsplit(uniqueVars, "|", fixed = TRUE)
  maxLen <- max(sapply(splitList, length))
  mat <- t(sapply(splitList, function(x) x[seq_len(maxLen)]))
  splitMatrixAsList <- function(mat, prefix) {
    value <- NULL
    if (prefix %in% uniqueVars) {
      value <- switch(
        entry,
        name = prefix,
        count = varsTable[prefix])
    }
    if (length(mat) == 0 || all(is.na(mat))) return(value)
    lst <- split(mat[, -1, drop = FALSE], mat[, 1])
    lst <- lapply(lst, matrix, ncol = NCOL(mat) - 1)
    newPrefixes <- paste0(prefix, if (nchar(prefix) > 0) "|", names(lst))
    return(c(value, mapply(splitMatrixAsList, lst, newPrefixes, SIMPLIFY = FALSE)))
  }
  res <- splitMatrixAsList(mat, "")
  return(res)
}
