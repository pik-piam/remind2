#' Read a finished report and calculate variables that need not only variables produced by other
#' report*.R functions, (from convGDX2MIF.R), but also variables coming from submodels for
#' which the reporting is done outside of it, such as EDGE-T and MAGICC.
#' #'
#'
#' @param inmifpath a character string with the path to a REMIND mif file, that
#' should contain only one model and scenario, and already have both the REMIND and EDGE-T variables
#'
#' @author Gabriel Abrahao
#' @export
#' @importFrom assertr assert not_na
#' @importFrom gdx readGDX
#' @importFrom magclass getYears getRegions mbind setNames mselect
#' new.magpie setYears mcalc dimReduce
#' @importFrom tibble as_tibble
#' @importFrom tidyselect everything
#' @importFrom tidyr everything
#' @importFrom madrat readSource toolGetMapping toolAggregate
#' @importFrom mrcommons everything
#' @importFrom piamutils deletePlus
#'

reportCrossSubmodels <- function(inmifpath) {

  # Read the REMIND mif. It should contain only one model and scenario, and already have the EDGE-T variables
  inreport <- read.report(inmifpath, as.list = FALSE)
  reglist <- getItems(inreport, dim = 1)

  ## delete "+" and "++" from variable names
  inreport <- deletePlus(inreport)

  # Drop the global region. convertCEDS2024 already spreads aviation and shipping emissions
  # to all regions equally, so totals are preserved
  inreport <- inreport[setdiff(reglist, "GLO"), , ]
  # Also drop scenario and model
  inreport <- dimReduce(inreport)


  # Supported regional mappings to try
  supportedregmappings <- c("regionmappingH12.csv", "regionmapping_21_EU11.csv")

  # Check which regional mapping has the same regions as the report and read it
  supportedreglists <- lapply(supportedregmappings, \(x) {
    unique(toolGetMapping(x, type = "regional", where = "mappingfolder")[, "RegionCode"])
  })
  indregmap <- which(lapply(supportedreglists, \(x) length(setdiff(x, reglist))) == 0)
  if (length(indregmap) != 1) {
    cat("Tried to find a regional mapping for the regions in the report, but either none or too many were found among the supported ones:\n")
    cat(supportedregmappings, "\n")
    cat("Regions in the report:", reglist, "\n")
    stop("No supported regional mapping found for the regions in the report")
  }
  regmap <- toolGetMapping(supportedregmappings[indregmap], type = "regional", where = "mappingfolder")


  # Sectoral mappings
  # Mapping between CEDS sectors and some IAMC sectors we want to estimate emission factors for
  iamcsectormap <- toolGetMapping("mappingCEDS2024toREMIND.csv", type = "sectoral", where = "mrremind")



  # Read emissions from CEDS to derive emission factors
  incedsall <- readSource("CEDS2024")

  # Aggregate CEDS emissions to inferred REMIND regions
  cedsreg <- toolAggregate(incedsall, rel = regmap, dim = 1, from = "RegionCode", to = "CountryCode")
  # Discard nonmapped sectors and aggregate them
  cedsreg <- cedsreg[, , intersect(getItems(cedsreg, dim = 3.1), iamcsectormap$CEDS2024)]
  cedsiamc <- toolAggregate(cedsreg, rel = iamcsectormap, dim = 3.1, from = "CEDS2024", to = "IAMC")

  # Calculate emissions that are based on emission factors. Derive EFs based on
  # CEDS 2020 emissions and REMIND 2020 activities

  MtN_to_ktN2O <- 44 / 28 * 1000 # conversion from MtN to ktN2O
  # Output magclass object
  out <- NULL

  # N2O from international shipping
  ef <- setYears(
    dimReduce(cedsiamc[, 2020, "International Shipping.n2o_n"]) /
      inreport[, 2020, "ES|Transport|Bunkers|Freight (billion tkm/yr)"], NULL
  )
  out <- mbind(
    out,
    setNames(
      inreport[, , "ES|Transport|Bunkers|Freight (billion tkm/yr)"] * ef * MtN_to_ktN2O,
      "Emi|N2O|Extra|Transport|Bunkers|Freight (kt N2O/yr)"
    )
  )
  # CH4 from international shipping (should be very small)
  ef <- setYears(
    dimReduce(cedsiamc[, 2020, "International Shipping.ch4"]) /
      inreport[, 2020, "ES|Transport|Bunkers|Freight (billion tkm/yr)"], NULL
  )
  out <- mbind(
    out,
    setNames(
      inreport[, , "ES|Transport|Bunkers|Freight (billion tkm/yr)"] * ef,
      "Emi|CH4|Extra|Transport|Bunkers|Freight (Mt CH4/yr)"
    )
  )
  # N2O from domestic+international aviation
  ef <- setYears(
    dimReduce(cedsiamc[, 2020, "Aircraft.n2o_n"]) /
      inreport[, 2020, "ES|Transport|Pass|Aviation (billion pkm/yr)"], NULL
  )
  out <- mbind(
    out,
    setNames(
      inreport[, , "ES|Transport|Pass|Aviation (billion pkm/yr)"] * ef * MtN_to_ktN2O,
      "Emi|N2O|Extra|Transport|Pass|Aviation (kt N2O/yr)"
    )
  )
  # CH4 from residential+commercial, assume most of it is from gas use. Requires CEDS detail
  ef <- setYears(
    dimReduce(cedsreg[, 2020, "1A4a_Commercial-institutional.ch4"] + cedsreg[, 2020, "1A4b_Residential.ch4"]) /
      inreport[, 2020, "FE|Buildings|Gases|Fossil (EJ/yr)"], NULL
  )
  out <- mbind(
    out,
    setNames(
      inreport[, , "FE|Buildings|Gases|Fossil (EJ/yr)"] * ef,
      "Emi|CH4|Extra|Buildings|Gases|Fossil (Mt CH4/yr)"
    )
  )
  # N2O from residential+commercial. Requires CEDS detail, assume it's all from fuel burning byproducts.
  # Common solid fuels tend to have higher N2O EFs than common gaseous and liquid fuels, but here
  # we are implicitly assuming the 2020 mix Solids+Liquids+Gases determines the EF.
  # See https://www.epa.gov/system/files/documents/2024-02/ghg-emission-factors-hub-2024.pdf
  tmp <- dimSums(inreport[, , c("FE|Buildings|Gases (EJ/yr)", "FE|Buildings|Liquids (EJ/yr)", "FE|Buildings|Solids (EJ/yr)")], dim = 3)
  ef <- setYears(
    dimReduce(cedsreg[, 2020, "1A4a_Commercial-institutional.ch4"] + cedsreg[, 2020, "1A4b_Residential.ch4"]) /
      tmp[, 2020, ], NULL
  )
  out <- mbind(
    out,
    setNames(
      tmp * ef,
      "Emi|N2O|Extra|Buildings|Gases|Fossil (Mt CH4/yr)"
    )
  )

  # Aggregate to global. Since all variables are emissions, we can just sum them
  out <- mbind(out, setItems(dimSums(out, dim = 1), dim = 1, value = "GLO"))


  return(out)
}
