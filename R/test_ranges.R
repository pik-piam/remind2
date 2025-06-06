#' Test Ranges on Variables in magpie or quitte Objects
#'
#' @md
#' @param data A [`magpie`][magclass::magclass] or quitte object to test.
#' @param tests A list of tests to perform, where each tests consists of:
#'     - A regular expression to match variables names in `data` (mandatory
#'       first item).
#'     - A named entry `low` to test the lower bound.  Can be set to `NULL` or
#'       omitted.
#'     - A named entry `up` to test the upper bound.  Can be set to `NULL` or
#'       omitted.
#'     - An optional entry `ignore.case` which can be set to `FALSE` if the
#'       regular expression should be matched case-sensitive.
#' @param reaction A character string, either `'warning'` or `'stop'`, to either
#'     warn or throw an error if variables exceed the ranges.
#' @param report.missing If set to `TRUE`, will message about regular
#'     expressions from `tests` not matching any variables in `data`.
#'
#' @author Michaja Pehl, Falk Benke
#'
#' @examples
#' require(dplyr)
#' require(tidyr)
#'
#' (data <- bind_rows(
#'   expand_grid(variable = 'Foo Share (%)',
#'               value = c(-1, 0, 42, 100, 101)),
#'   expand_grid(variable = 'bar share (percent)',
#'               value = c(-1, 0, 42, 100, 101))) %>%
#'   group_by(variable) %>%
#'   mutate(year = 2000 + (1:n())) %>%
#'   ungroup() %>%
#'   select(year, variable, value) %>%
#'   as.magpie(spatial = 0, temporal = 1, datacol = 3))
#'
#' tests <- list(list("Share.*\\((%|Percent)\\)$", low = 0, up = 100))
#'
#' test_ranges(data, tests)
#'
#' @importFrom dplyr distinct filter last pull
#' @importFrom quitte magclass_to_tibble
#' @importFrom tidyr unite
#' @importFrom tidyselect everything

#' @export
test_ranges <- function(data, tests, reaction = c('warning', 'stop'),
                        report.missing = FALSE) {

  match.arg(reaction)

  if (!(   is.list(tests)
        && all(sapply(tests, is.list))
        && all(sapply(tests, function(x) { is.character(x[[1]]) })))) {
    stop('`tests` must be a list of lists, and the first element of each list ',
         'must be a string.')
  }

  # Test all variables in data against low and up
  .test <- function(data, low, up) {

    low_data <- if (!is.null(low) && any(data$value < low)) {
      data %>%
        filter(.data$value < low) %>%
        distinct(.data$variable, .keep_all = TRUE) %>%
        unite('text', everything(), sep = '   ') %>%
        pull('text')
    }
    else {
      character()
    }

    up_data <- if (!is.null(up) && any(data$value > up)) {
      data %>%
        filter(.data$value > up) %>%
        distinct(.data$variable, .keep_all = TRUE) %>%
        unite('text', everything(), sep = '   ') %>%
        pull('text')
    }
    else {
      character()
    }

    if (0 != length(low_data))
      low_data <- c(paste('variables exceeding lower limit', low), low_data)
    if (0 != length(up_data))
      up_data <- c(paste('variables exceeding upper limit', up), up_data)

    return(list(low_data, up_data))
  }

  if (is.magpie(data)) {
    data_names <- last(getNames(data, fulldim = TRUE))
  } else if (quitte::is.quitte(data)) {
    data_names <- unique(paste0(data$variable, " (", data$unit, ")"))
  } else {
    stop("data must bei either quitte or magpie object")
  }

  msg <- list()
  for (t in tests) {
    # if t has no 'ignore.case' element, or it is not set to FALSE, do not
    # ignore the case
    ignore.case <- !'ignore.case' %in% names(t) || !isFALSE(t$ignore.case)
    variables <- grep(t[[1]], data_names, ignore.case = ignore.case,
                      value = TRUE)

    if (isTRUE(report.missing) && 0 == length(variables)) {
      message('No variables match regex "', t[[1]], '"')
    }

    if (is.magpie(data)) {
      df <- data[, , variables] %>%
        magclass_to_tibble()
    } else {
      df <- data %>% filter(
        .data$variable %in% magclass::unitsplit(variables)$variable
      )
    }

    msg <- append(
      msg,
      .test(df, getElement(t, 'low'), getElement(t, 'up')) %>%
        Filter(function(x) { 0 != length(x) }, x = .) %>%
        lapply(paste, collapse = '\n')
    )
  }

  if (length(msg)) {
    msg <- paste('range error\n', msg, collapse = '\n')
    if ('warning' == reaction[[1]]) {
      warning(msg)
    }
    else {
      stop(msg)
    }
  } else {
    message("All range checks were fine.")
  }

  return(msg)
}
