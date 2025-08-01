# uncomment to skip test
# skip("Skip GDX test")

# Check REMIND output. dt is a data.table in *wide* format,
# i.e., variables are columns. `eqs` is a list of equations of the form
# list(LHS = "RHS", ...). The scope determines if the equations
# should be checked for regions ("regional"), only globally ("world") or
# both ("all"). Sensitivity determines the allowed offset when comparing
# LHS to RHS
library(dplyr)
library(gdx)

test_that("Test if REMIND reporting is produced as it should and check data integrity", {
  skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")

  # GDXs for comparison.
  gdxList <- c("fulldata-SSP2-EU21-PkBudg650-release.gdx" = "https://rse.pik-potsdam.de/data/example/remind2_test-convGDX2MIF_SSP2-EU21-PkBudg650_2025-04-07_14.31.18.gdx",
               "fulldata-SSP2-NPi-AMT.gdx"                = "https://rse.pik-potsdam.de/data/example/remind2_test-convGDX2MIF_SSP2-NPi-AMT.gdx")

  gdxPaths <- NULL
  for (i in seq_along(gdxList)) {
    from <- gdxList[i]
    to   <- file.path(tempdir(), names(gdxList[i]))
    if (!file.exists(to)) {
      utils::download.file(from, to, mode = "wb", quiet = TRUE)
    }
    gdxPaths <- c(gdxPaths, to)
  }

  checkPiamTemplates <- function(computedVariables) {
    # if you add a new template here, make sure to adjust the piamInterfaces version in the DESCRIPTION
    templates <- c("AR6", "AR6_NGFS", "ELEVATE", "NAVIGATE", "SHAPE", "ARIADNE", "ECEMF", "ScenarioMIP")
    templateVariables <- templates %>%
      piamInterfaces::getREMINDTemplateVariables() %>%
      deletePlus() %>%
      unique()
    expect_true(any(computedVariables %in% templateVariables))
    missingVariables <- setdiff(templateVariables, computedVariables)
    if (length(missingVariables) > 0) {
      warning("The following variables are expected in the piamInterfaces package ",
              "for templates ", paste(templates, collapse = ", "),
              ",\nbut cannot be found in the reporting generated by ", gdxPath, ":\n ",
              paste(missingVariables, collapse = ",\n "),
              "\nTo find out where this variable is used, run: piamInterfaces::variableInfo('",
              magclass::unitsplit(missingVariables[[1]])$variable, "')",
              "\nPossible reasons are:\n",
              "- A variable was renamed in piamInterfaces, but not yet in the reporting, or inversely.\n",
              "  To rename in piamInterfaces, see https://github.com/pik-piam/piamInterfaces/blob/master/tutorial.md#renaming-a-piam_variable\n",
              "- A variable was removed from remind2. Then, it needs to be replaced or removed from piamInterfaces as well.\n",
              "- A variable is marked mandatory in piamInterfaces, while it is only reported for special REMIND settings/realizations.\n",
              "  Add an 'x' to the 'source' column in the mappings.")
    }
  }

  # uncomment to add current calibration gdxes
  # gdxPaths <- c(gdxPaths, Sys.glob("/p/projects/remind/inputdata/CESparametersAndGDX/*.gdx"))

  numberOfMifs <- 0

  for (gdxPath in gdxPaths) {
    numberOfMifs <- numberOfMifs + 1

    message("Running convGDX2MIF(", gdxPath, ")...")
    refpolicycost <- if (gdxPath == gdxPaths[[1]]) gdxPath else NULL
    mifContent <- convGDX2MIF(gdxPath, gdx_refpolicycost = refpolicycost, testthat = TRUE, extraData = NULL)

    expect_no_warning(piamInterfaces::checkVarNames(getNames(mifContent, dim = 3)))

    if (!grepl("release", gdxPath)) {
      computedVariables <- deletePlus(getItems(mifContent, dim = 3.3))
      computedVariables <- gsub("\\(\\)", "(unitless)", computedVariables)
      checkPiamTemplates(computedVariables)
    }

    expect_no_error(
      test_ranges(
        data = mifContent,
        tests = list(
          list(
            "^Emi\\|CO2\\|Energy\\|Demand\\|Industry\\|.*Fossil \\(Mt CO2/yr\\)$",
            low = 0),
          list("Share.*\\((%|Percent)\\)$", low = 0, up = 100)),
        reaction = "stop"))

    magclass::write.report(
      x = magclass::collapseNames(mifContent),
      file = file.path(tempdir(), paste0(numberOfMifs, ".mif")),
      scenario = paste0(magclass::getItems(mifContent, dim = "scenario"), numberOfMifs),
      model = "REMIND"
    )
  }

  unlink(tempdir(), recursive = TRUE)
  tempdir(TRUE)
})
