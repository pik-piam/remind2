#' Perform one or more summation checks and write results to file or attribute
#'
#' Perform summation checks on mifData for each file given in summationsFile
#' and write results to file or attach as attribute to mifData
#'
#' @param mifData object containing all the reporting information
#' @param file name of a file the summation check results should be written to. If NULL results will be attached as attribute to mifData
#' @param summationsFile single or multiple (as vector) files that describes the required summation groups (see 
#' @param testthat boolean whether called by tests, turns some messages into warnings
#' @returns the object given in mifData with results of summation checks attached as attribute if 
#'
#' @author David Klein, Oliver Richters, Michaja Pehl
#'
#' @export
#' @importFrom dplyr %>% bind_rows filter
#' @importFrom piamInterfaces checkSummations
#' @importFrom utils write.csv

.reportSummationErrors <- function(msg, testthat) {
    if (!any(grepl('All summation checks were fine', msg))) {
      msgtext <- paste(msg, collapse = '\n')
      if (isTRUE(testthat)) warning(msgtext) else message(msgtext)
    }
  }

checkSummationsMult <- function(mifData, file = NULL, summationsFile = "extractVariableGroups", testthat = FALSE) {
    sumChecks <- NULL
    capture.output(
      for (sF in summationsFile) {
        tmp <- checkSummations(mifFile = mifData, dataDumpFile = NULL, outputDirectory = NULL,
                               summationsFile = sF, absDiff = 1.5e-8, relDiff = 1e-8, roundDiff = TRUE)
        sumChecks <- bind_rows(sumChecks, tmp)
      },
      type = 'message'
    ) %>% 
    .reportSummationErrors(testthat = testthat)
    if (!is.null(sumChecks)) sumChecks <- filter(sumChecks, abs(.data$diff) >= 1.5e-8)

    # report results
    if (isTRUE(nrow(sumChecks) > 0)) {
      if (is.null(file)) {
        # return summation errors as attribute
        warning("Summation checks have revealed some gaps! See `summation_errors` attribute on mifData for details.")
        attr(mifData, 'summation_errors') <- sumChecks
      } else {
        # write summation errors to file
        summation_errors_file <- sub('(\\.[^.]+)$', '_summation_errors.csv', file)
        warning("Summation checks have revealed some gaps! See file ", summation_errors_file)
        write.csv(sumChecks, summation_errors_file, quote = FALSE, row.names = FALSE)
      }
    }
    return(mifData)
  }
