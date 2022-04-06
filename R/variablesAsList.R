#' Variable Names as Hierarchical List
#'
#' Take a character vector of variables names structured into groups and
#' subgroups by \code{|} in the name and convert it into a hierarchical
#' structure of named lists.
#'
#' @param v A character vector of variable names. Hierarchical section structure
#'   of variables is declared by the symbol \code{|}.
#' @return A hierarchical named list with the structure defined by the use of
#'   \code{|}. All leaf entries are \code{NULL}.
#' @author Christof Schoetz
#' @examples
#' vars <- c(
#'   "Emi|GHG|CO2", "Emi|GHG|CH4", "Emi|NOX",
#'   "FE", "FE|Buildings", "FE|Industry", "FE|Transport")
#' varList <- variablesAsList(vars)
#' \dontrun{View(varList)}
variablesAsList <- function(v) {
  splitList <- strsplit(v, "|", fixed=TRUE)
  maxLen <- max(sapply(splitList, length))
  mat <- t(sapply(splitList, function(x) x[seq_len(maxLen)]))
  splitMatrixAsList <- function(mat) {
    if (length(mat) == 0 || all(is.na(mat))) return(NULL)
    lst <- split(mat[,-1,drop=FALSE], mat[,1])
    lst <- lapply(lst, matrix, ncol = NCOL(mat)-1)
    return(lapply(lst, splitMatrixAsList))
  }
  res <- splitMatrixAsList(mat)
  return(res)
}
