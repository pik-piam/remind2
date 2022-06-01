#' Perform summation check on a dataset.
#'
#' Based on the |+| notation in variable names.
#'
#'
#' @param x a quitte object containing the data
#' @author Anastasis Giannousakis
#' @examples
#'
#' \dontrun{
#' checkSummations(x)
#' }
#'
#' @return a list with the failed and/or missing variable groups as elements
#' @importFrom mip extractVariableGroups
#' @importFrom dplyr %>% filter group_by summarise select arrange
#' @export

checkSummations <- function(x) {
  sgroup <- extractVariableGroups(levels(x$variable), keepOrigNames = TRUE)
  failed <- NULL
  variable <- NULL
  region <- NULL
  value <- NULL
  period <- NULL

  if (length(sgroup) > 0) {
    for (i in 1:length(sgroup)) {
      if (!names(sgroup[i]) %in% unique(x$variable)) { # variable total missing in dataset
        failed <- c(failed, names(sgroup[i]))
      } else { # summation group does not sum up
        if (any(abs(filter(x, variable%in%sgroup[[i]]) %>%
                    group_by(region, period) %>%
                    summarise(grsum = sum(value), .groups = "drop") %>%
                    select("grsum") -
                    filter(arrange(x, region, period), variable == names(sgroup[i]))[["value"]])

                >

                0.00001)

        ) failed <- c(failed, names(sgroup[i]))

      }

    }
    ifelse(is.null(failed), message("Summations look good"), return(list("failing and/or missing variable groups", failed)))
  } else {
    message("No summation groups found in the data")
  }

}
