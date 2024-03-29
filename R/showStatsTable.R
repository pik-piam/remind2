#' Show a Table of a Statistics Data Frame
#'
#' Take the output of \code{\link{calcTimeSeriesStats}} (a data frame) and show
#' it as a kable table. Only results from model REMIND are shown. It is assumed
#' that the input argument only contains a single statistic of a single variable
#' (but multiple regions and scenarios). The caption of the resulting table is
#' created from the entries of the columns variable, unit, and statistic in the
#' first row (should be the same in all rows).
#'
#' @param statsData A data frame with the columns "model", "scenario", "region",
#'   "value", "variable", "unit", "statistic".
#' @param mainReg \code{character(1)}. The main region. Will be shown at the top
#'   of the table.
#' @return A \code{knitr_kable} object. A table with columns region and one
#'   column for each scenario.
#' @examples
#'   \dontrun{
#'   data %>%
#'     calculateStatisticsOfTimeSeries(
#'       "Emi|CO2",
#'       stats = list("Net 0 Period" = function(v, p) p[which(v <= 0)[1]])
#'     )  %>%
#'     showStatsTable()
#'   }
#' @importFrom dplyr %>% filter select arrange
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @export
showStatsTable <- function(statsData, mainReg = getOption("mip.mainReg")) {

  if (!requireNamespace("kableExtra", quietly = TRUE))
    stop("Package \"kableExtra\" must be installed to use showStatsTable().", call. = FALSE)

  stopifnot(is.data.frame(statsData))
  stopifnot(c("model", "scenario", "region", "value", "variable", "unit", "statistic") %in% colnames(statsData))

  if (nrow(statsData) == 0) {
    warning("Empty table.")
    return(NULL)
  }

  p <- statsData %>%
    filter(.data$model == "REMIND") %>%
    select(.data$scenario, .data$region, .data$value) %>%
    arrange(.data$region != mainReg, .data$region)  %>%
    pivot_wider(names_from = .data$scenario, values_from = .data$value)

  firstColumnWidth <- 20
  pageTextWidth <- 287
  cellMargin <- 1

  p %>%
    kableExtra::kbl(
      caption = paste0(statsData$variable[1], " [", statsData$unit[1], "]: ", statsData$statistic[1]),
      booktabs = TRUE,
      align = c("l", rep("r", ncol(p) - 1))
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "condensed"),
      latex_options = c("striped", "HOLD_position"),
      full_width = FALSE
    ) %>%
    kableExtra::column_spec(
      1,
      width = paste0(firstColumnWidth, "mm"),
      bold = TRUE,
    ) %>%
    kableExtra::column_spec(
      2:ncol(p),
      width = paste0((pageTextWidth - firstColumnWidth) / ncol(p) - cellMargin, "mm"))
}
