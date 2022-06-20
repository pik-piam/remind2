#' Perform summation check on a dataset.
#'
#' Based on the |+| notation in variable names.
#'
#'
#' @param x a quitte object containing the data
#' @param tolerance a numeric value of accepted tolerance for parent = sum(children)
#' @author Anastasis Giannousakis
#' @examples
#'
#' \dontrun{
#' checkSummations(x)
#' }
#'
#' @return a quitte with the failed and/or missing variable groups (contains a
#' column "parent" to group by)
#' @importFrom mip extractVariableGroups
#' @importFrom dplyr %>% filter group_by summarise select arrange mutate
#' @importFrom quitte is.quitte
#' @export

checkSummations <- function(x, tolerance = 0.00001) {

  if (!is.quitte(x)) stop("Object must be quitte")

  tmp <- read.csv(system.file("extdata", "summationGroups.csv", package = "remind2"), sep = ";", stringsAsFactors = FALSE)
  sgroup <- NULL
  for (i in unique(tmp[, "parent"])) sgroup[[i]] <- tmp[which(tmp[, "parent"]==i), "child"]

  sgroup <- c(sgroup, extractVariableGroups(levels(x$variable), keepOrigNames = TRUE))
  failed <- NULL
  variable <- NULL
  region <- NULL
  value <- NULL
  period <- NULL
  parent <- NULL

  if (length(sgroup) > 0) {
    for (i in seq_len(length(sgroup))) {
      if (!names(sgroup[i]) %in% unique(x$variable)) { # variable total missing in dataset
        failed <- c(failed, names(sgroup[i]))
      } else { # summation group does not sum up
        if (any(abs(filter(x, variable %in% sgroup[[i]]) %>%
                    group_by(region, period) %>%
                    summarise(grsum = sum(value), .groups = "drop") %>%
                    select("grsum") -
                    filter(arrange(x, region, period), variable == names(sgroup[i]))[["value"]])

                >

                tolerance)

        ) {
          tmp <- x %>% filter(variable %in% sgroup[[i]]) %>% mutate(parent = names(sgroup[i]))
          failed <- rbind(failed, tmp)
        }

      }

    }
    if (is.null(failed)) {
      message("Summations look good")
    } else {
      return(failed)
    }
  } else {
    message("No summation groups found in the data")
  }

}
