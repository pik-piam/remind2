#' @export

# Check REMIND output. dt is a data.table in *wide* format,
# i.e., variables are columns. `eqs` is a list of equations of the form
# list(LHS = "RHS", ...). The scope determines if the equations
# should be checked for regions ("regional"), only globally ("world") or
# both ("all"). Sensitivity determines the allowed offset when comparing
# LHS to RHS
checkEqs <- function(dt, eqs, scope = "all", sens = 1e-8) {
  if (scope == "regional") {
    dt <- dt[all_regi != "World"]
  } else if (scope == "world") {
    dt <- dt[all_regi == "World"]
  }

  for (LHS in seq_along(names(eqs))) {
    exp <- parse(text = eqs[[LHS]])
    dt[, total := eval(exp), by = .(all_regi, ttot, scenario, model)]

    dt[, diff := total - get(names(eqs)[LHS])]
    if (nrow(dt[abs(diff) > sens]) > 0) {
      warning(paste(c(paste("Check on data integrity failed for", names(eqs)[LHS]),
                   gsub('`', '', unlist(strsplit(eqs[[LHS]], '`+`', TRUE)))),
                 collapse = '\n' ))
    }
  }
}
