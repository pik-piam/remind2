#' Variable Names as Hierarchical List
#'
#' Take a character vector of variables names structured into groups and
#' subgroups by \code{|} in the name and convert it into a hierarchical
#' structure of named lists.
#'
#' The input data can be provided as a string of variables \code{vars}, a quitte
#' object \code{data}, or a mif-file \code{file}. Exactly one of those arguments
#' must be specified.
#'
#' @param vars A character vector of variable names. Hierarchical section
#'   structure of variables is declared by the symbol \code{|}.
#' @param file A single string, the path to a mif file, to be processed by
#'   \code{quitte::read_quitte()}.
#' @param data A quitte object from which the column \code{variable} is read and
#'   parsed.
#' @param entry A string determining the entries of all leafs and the nodes
#'   which represent variables in \code{vars}. \code{"NULL"} puts \code{NULL}
#'   into the leafs of the resulting list. \code{"name"} places the full name of
#'   an existing variable in the respective node. \code{"count"} shows the
#'   number of occurrences of the respective variable name in \code{vars} as the
#'   first entry of a node. \code{"list"} puts list entries \code{nm} and
#'   \code{count} inside each node that represents a variable occurring in
#'   \code{vars} with the full variable name and the number of occurrences,
#'   respectively. Furthermore, if values are provided with the variables using
#'   the arguments \code{file} or \code{data}, then the vector of values of this
#'   variable is put in the list as entry \code{val}.
#' @return A hierarchical named list with the structure defined by the use of
#'   \code{|}.
#' @author Christof Schoetz
#' @examples
#' vars <- c(
#'   "Emi|GHG|CO2", "Emi|GHG|CH4", "Emi|NOX",
#'   "FE", "FE|Buildings", "FE|Industry", "FE|Transport")
#' varList <- variablesAsList(vars)
#' \dontrun{View(varList)}
#' varList <- variablesAsList(vars, entry = "count")
#' \dontrun{View(varList)}
#'
#' \dontrun{
#' v <- variablesAsList(data$variable, entry = "list")
#' mip::showLinePlots(data, v$Emi$GHG$nm)
#' }
#'
#' \dontrun{View(variablesAsList("path/to/scenario.mif"))}
#'
#' \dontrun{
#' data %>% # make values uniquely identified by variable name
#'   filter(period == 2100, region == "World") %>%
#'   variablesAsList(data = ., entry = "list") ->
#'   v
#' # Use v to explore and check the values.
#' }
variablesAsList <- function(
  vars,
  file,
  data,
  entry = c("NULL", "name", "count", "list")
) {

  entry <- match.arg(entry)

  if (sum(!c(missing(vars), missing(file), missing(data))) != 1) {
    stop("Need to provide exectly one of vars, file, data.")
  }

  if (!missing(vars)) {
    vars <- as.character(vars)
    data <- NULL
  } else if (!missing(file)) {
    data <- quitte::read.quitte(vars)
    vars <- data$variable
  } else if (!missing(data)) {
    data <- quitte::as.quitte(data)
    vars <- data$variable
  }

  varsTable <- table(vars)
  uniqueVars <- names(varsTable)
  splitList <- strsplit(uniqueVars, "|", fixed = TRUE)
  maxLen <- max(sapply(splitList, length))
  varsMatrix <- t(sapply(splitList, function(x) x[seq_len(maxLen)]))

  # Function to recursively parse variable names and create node value.
  .splitMatrixAsList <- function(mat, prefix) {

    # Set the value of the node.
    if (prefix %in% uniqueVars) {
      nodeValue <- switch(
        entry,
        "NULL" = NULL,
        "name" = prefix,
        "list" = list(
          nm = prefix,
          count = unname(varsTable[prefix]),
          val = if (is.null(data)) NULL else data$value[data$variable == prefix]),
        "count" = varsTable[prefix])
    } else {
      cnt <- 0
      names(cnt) <- prefix
      nodeValue <- switch(
        entry,
        "NULL" = NULL,
        "name" = NULL,
        "list" = NULL,
        "count" = if (nchar(prefix) > 0) cnt else NULL)
    }

    # Termination condtion.
    if (length(mat) == 0 || all(is.na(mat))) return(nodeValue)

    # Process subcategories.
    lst <- split(mat[, -1, drop = FALSE], mat[, 1])
    lst <- lapply(lst, matrix, ncol = NCOL(mat) - 1)
    newPrefixes <- paste0(prefix, if (nchar(prefix) > 0) "|", names(lst))
    resSubCategories <- mapply(.splitMatrixAsList, lst, newPrefixes, SIMPLIFY = FALSE)

    return(c(nodeValue, resSubCategories))
  }

  res <- .splitMatrixAsList(varsMatrix, prefix = "")
  return(res)
}
