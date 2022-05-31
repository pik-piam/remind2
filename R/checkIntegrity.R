#' @export

checkIntegrity <- function(out) {
  dt <- rmndt::magpie2dt(out)
  stopifnot(!(c("total", "diff") %in% unique(dt[["variable"]])))
  dtWide <- data.table::dcast(dt, ... ~ variable)
  myList <- mip::extractVariableGroups(unique(dt[["variable"]]), keepOrigNames = TRUE)
  myList <- lapply(myList, FUN = function(x) paste0("`", x, "`"))
  myList <- lapply(myList, paste, collapse = "+")
  # remove from the tests the variables whose totals cannot be found
  chck <- grep(" \\(.*.\\)$", names(myList), invert = T)
  if (length(chck) > 0) {
    warning(paste0("For this group the corresponding total could not be found and the summation check ",
                   "will not be performed: \n", myList[chck], "\n\n"))
  }
  myList <- myList[grep(" \\(.*.\\)$", names(myList))]

  checkEqs(dtWide, myList)
}
