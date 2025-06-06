
library(quitte)
library(piamInterfaces)
library(dplyr)
path <- "~/Cluster/reporting_example/REMIND_generic_SSP2-PkBudg650-AMT.mif"

validateMif <- function(path){

  output <- read.quitte(path, check.duplicates = F)

  # check variable names ----

  varUnit <- output %>%
    mutate("varUnit" = paste0(.data$variable, " (", .data$unit, ")")) %>%
    pull("varUnit") %>%
    unique()

  piamInterfaces::checkVarNames(varUnit, withunits = TRUE)

  # check piam templates ----

  piamChecks <- remind2::checkPiamTemplates(computedVariables = deletePlus(varUnit), source = path)

  # summation checks ----

  sumChecks <- piamInterfaces::checkSummations(
    mifFile = output, dataDumpFile = NULL, outputDirectory = NULL,
    summationsFile = "extractVariableGroups",
    absDiff = 0.01, relDiff = 0.02, roundDiff = TRUE
  )

  sumChecks <- piamInterfaces::checkSummations(
    mifFile = output, dataDumpFile = NULL, outputDirectory = NULL,
    summationsFile = system.file("extdata/additional_summation_checks.csv",
                                 package = "remind2"),
    absDiff = 0.01, relDiff = 0.02, roundDiff = TRUE) %>%
    bind_rows(sumChecks)

  # range checks ----

  rangeChecks <- remind2::test_ranges(
    data = output,
    tests = list(
      list("^Emi\\|CO2\\|Energy\\|Demand\\|Industry\\|.*Fossil \\(Mt CO2/yr\\)$", low = 0),
      list("Share.*\\((%|Percent)\\)$", low = 0, up = 100)),
    reaction = "warning")

}

