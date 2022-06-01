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
#' @importFrom dplyr %>% filter group_by summarise select arrange mutate
#' @export

checkSummations <- function(x) {

  tmp <- read.csv(system.file("extdata", "summationGroups.csv", package = "remind2"), sep = ";", stringsAsFactors=FALSE)
  sgroup <- NULL
  for (i in unique(tmp[,"parent"])) sgroup[[i]] <- tmp[which(tmp[,"parent"]==i),"child"]

  sgroup <- c(sgroup, extractVariableGroups(levels(x$variable), keepOrigNames = TRUE))
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
    if (is.null(failed)) {
      message("Summations look good")
    } else {
      warning("Some variable groups do not sum up or the total is missing")
      return(list("failing and/or missing variable groups", failed))
    }
  } else {
    message("No summation groups found in the data")
  }

}
