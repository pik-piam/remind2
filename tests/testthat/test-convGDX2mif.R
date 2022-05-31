# uncomment to skip test
# skip("Skip GDX test")




test_that("Test if REMIND reporting is produced as it should and check data integrity", {
  skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")

  # add GDXs for comparison here:
  gdxPaths <- NULL

  if (length(gdxPaths) == 0) {
    defaultGdxPath <- file.path(tempdir(), "fulldata.gdx")
    if (!file.exists(defaultGdxPath)) {
      utils::download.file("https://rse.pik-potsdam.de/data/example/remind2_test-convGDX2MIF_fulldata.gdx",
                           defaultGdxPath, mode = "wb", quiet = TRUE)
    }
    gdxPaths <- defaultGdxPath
  }

  numberOfMifs <- 0
  for (gdxPath in gdxPaths) {
    numberOfMifs <- numberOfMifs + 1
    message("Running convGDX2MIF(", gdxPath, ") and checking integrity...")
    mifContent <- convGDX2MIF(gdxPath)
#    checkIntegrity(mifContent)
    magclass::write.report(
      x = magclass::collapseNames(mifContent),
      file = file.path(tempdir(), paste0(numberOfMifs, ".mif")),
      scenario = paste0(magclass::getItems(mifContent, dim = "scenario"), numberOfMifs),
      model = "REMIND"
    )
  }
  # create a second file, so we can actually check the comparison code
  if (numberOfMifs == 1) {
    numberOfMifs <- numberOfMifs + 1
    magclass::write.report(
      x = magclass::collapseNames(mifContent),
      file = file.path(tempdir(), paste0(numberOfMifs, ".mif")),
      scenario = paste0(magclass::getItems(mifContent, dim = "scenario"), numberOfMifs),
      model = "REMIND"
    )
  }

  message("Checking compareScenarios...")
  myMifs <- file.path(tempdir(), paste0(seq_len(numberOfMifs), ".mif"))
  histMif <- file.path(tempdir(), "historical.mif")
  if (!file.exists(histMif)) {
    utils::download.file("https://rse.pik-potsdam.de/data/example/historical.mif", histMif, quiet = TRUE)
  }
  capture.output( # Do not show stdout text.
    compareScenarios2(
      mifScen = myMifs,
      mifHist = histMif,
      outputFormat = "pdf",
      outputFile = "cs2_test",
      outputDir = tempdir(),
      sections = 0)) # Render only the info section.
  expect_true(file.exists(file.path(tempdir(), "cs2_test.pdf")))
  unlink(tempdir(), recursive = TRUE)
  tempdir(TRUE)
})
