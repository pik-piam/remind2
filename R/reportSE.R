#' Read in GDX and calculate secondary energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in secondary energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Gunnar Luderer, Lavinia Baumstark, Felix Schreyer
#' @examples
#' \dontrun{
#' reportSE(gdx)
#' }
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mselect getSets getSets<- getYears dimSums getNames<- mbind replace_non_finite
#' @importFrom abind abind

reportSE <- function(gdx, regionSubsetList = NULL, t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {

  ####### get realisations #########
  module2realisation <- readGDX(gdx, "module2realisation")
  rownames(module2realisation) <- module2realisation$modules

  ####### get power realisations #########
  realisation <- readGDX(gdx, "module2realisation")
  power_realisation <- if ("power" %in% realisation[, 1]) realisation[which(realisation[, 1] == "power"), 2]

  ####### conversion factors ##########
  pm_conv_TWa_EJ <- 31.536
  ####### read in needed data #########
  ## sets
  pe2se    <- readGDX(gdx, "pe2se")
  se2se    <- readGDX(gdx, "se2se")
  all_te    <- readGDX(gdx, "all_te")
  tefosccs <- readGDX(gdx, c("teFosCCS", "tefosccs"), format = "first_found")
  teccs    <- readGDX(gdx, c("teCCS", "teccs"), format = "first_found")
  tenoccs  <- readGDX(gdx, c("teNoCCS", "tenoccs"), format = "first_found")
  techp    <- readGDX(gdx, c("teChp", "techp"), format = "first_found")
  terenew_nobio <- readGDX(gdx, c("teReNoBio", "terenew_nobio"), format = "first_found")
  pebio    <- readGDX(gdx, c("peBio", "pebio"), format = "first_found")
  sety     <- readGDX(gdx, c("entySe", "sety"), format = "first_found")
  pety     <- readGDX(gdx, c("entyPe", "pety"), format = "first_found")
  oc2te    <- readGDX(gdx, c("pc2te", "oc2te"), format = "first_found")



  # the set liquids changed from sepet+sedie to seLiq in REMIND 1.7. Seliq, sega and seso changed to include biomass or Fossil origin after REMIND 2.0
  se_Liq    <- intersect(c("seliqfos", "seliqbio", "seliqsyn", "seliq", "sepet", "sedie"), sety)
  se_Gas    <- intersect(c("segafos", "segabio", "segasyn", "sega"), sety)
  se_Solids <- intersect(c("sesofos", "sesobio", "seso"), sety)

  # Gases and Liquids can also be made from H2 via CCU
  input_gas <- c(pety, "seh2")
  input_liquids <- c(pety, "seh2")

  ## parameter
  dataoc_tmp    <- readGDX(gdx, c("pm_prodCouple", "p_prodCouple", "p_dataoc"), restore_zeros = FALSE, format = "first_found")
  dataoc_tmp[is.na(dataoc_tmp)] <- 0
  p_macBase <- readGDX(gdx, c("p_macBaseMagpie", "p_macBase"), format = "first_found")
  #  p_macEmi  <- readGDX(gdx,"p_macEmi")
  ## variables
  prodSe <- readGDX(gdx, name = c("vm_prodSe", "v_seprod"), field = "l", restore_zeros = FALSE, format = "first_found") * pm_conv_TWa_EJ
  prodSe <- mselect(prodSe, all_enty1 = sety)

  if (length(tmp_d3 <- intersect(c("MeOH", "h22ch4"),
    getNames(prodSe, dim = 3)))) {
    # if synfuels are activated, there might be no demand until 2020. This can
    # lead to NAs that need to be substituted with 0
    prodSe[, c("y2005", "y2010", "y2015", "y2020"), tmp_d3] <-
      replace_non_finite(prodSe[, c("y2005", "y2010", "y2015", "y2020"), tmp_d3])
  }

  #  storloss only exist for versions previous to the power module creation and for the IntC power module realisation
  if ((is.null(power_realisation)) || (power_realisation == "IntC")) {
    storLoss <- readGDX(gdx, name = c("v32_storloss", "v_storloss"), field = "l", restore_zeros = TRUE, format = "first_found") * pm_conv_TWa_EJ
    # TODO: declare storLoss declared over all_te in the GAMS code (old coment?).
    getSets(storLoss)[3] <- "all_te"
    # calculate minimal temporal resolution #####
    y <- Reduce(intersect, list(getYears(prodSe), getYears(storLoss)))
  } else { # RLDC power module
    storLoss <- NULL
    y <- getYears(prodSe)
  }

  vm_macBase <- readGDX(gdx, name = c("vm_macBase"), field = "l", restore_zeros = FALSE, format = "first_found") * pm_conv_TWa_EJ
  vm_macBase <- vm_macBase[, y, ]
  vm_emiMacSector <- readGDX(gdx, name = c("vm_emiMacSector"), field = "l", restore_zeros = FALSE, format = "first_found") * pm_conv_TWa_EJ
  vm_emiMacSector <-   vm_emiMacSector[, y, ]
  ####### set temporal resolution #####
  prodSe    <- prodSe[, y, ]
  storLoss  <- storLoss[, y, ]
  p_macBase <- p_macBase[, y, ]
  ####### fix negative values to 0 ##################
  #### adjust regional dimension of dataoc
  dataoc <- new.magpie(getRegions(prodSe), getYears(dataoc_tmp), magclass::getNames(dataoc_tmp), fill = 0)
  dataoc[getRegions(dataoc_tmp), , ] <- dataoc_tmp
  getSets(dataoc) <- getSets(dataoc_tmp)

  dataoc[dataoc < 0] <- 0
  ###### include se2se technologies in summation
  te_pese2se <- c(pe2se$all_te, se2se$all_te)
  ####### internal function for reporting ###########
  se.prod <- function(prodSe, dataoc, oc2te, sety, enty.input, se.output, te = te_pese2se, name = NULL, storageLoss = storLoss, all_pety = pety) {

    # test if inputs make sense
    if (length(setdiff(enty.input, abind::abind(all_pety, sety))) > 0)
      warning(paste("Input energy enty.input ", setdiff(enty.input, abind::abind(all_pety, sety)), " is not element of pety or sety"))
    if (length(setdiff(se.output, abind::abind(all_pety, sety))) > 0)
      warning(paste("se.output ", setdiff(se.output, abind::abind(all_pety, sety)), " is not element of pety or sety"))
    ## identify all techs with secarrier as a main product
    # sub1_oc2te <- oc2te[(oc2te$all_enty %in% pecarrier) & (oc2te$all_enty1 %in% secarrier) & (oc2te$all_enty2 %in% sety)    & (oc2te$all_te %in% te),]
    ## secondary energy production with secarrier as a main product
    x1 <- dimSums(mselect(prodSe, all_enty = enty.input, all_enty1 = se.output, all_te = te), dim = 3, na.rm = T)
    ## secondary energy production with secarrier as a couple product
    ## identify all oc techs with secarrier as a couple product
    sub_oc2te <- oc2te[(oc2te$all_enty %in% enty.input) & (oc2te$all_enty1 %in% sety)    & (oc2te$all_enty2 %in% se.output) & (oc2te$all_te %in% te), ]
    x2 <- dimSums(prodSe[sub_oc2te] * dataoc[sub_oc2te], dim = 3, na.rm = T)

    ## storage losses
    input.pe2se <- pe2se[(pe2se$all_enty %in% enty.input) & (pe2se$all_enty1 %in% se.output) & (pe2se$all_te %in% te), ]
    if ((nrow(input.pe2se) == 0) || (is.null(storageLoss))) {
      x3 <- 0
    } else {
      x3 <- dimSums(storageLoss[input.pe2se], dim = 3, na.rm = T)
    }

    out <- (x1 + x2 - x3)

    if (!is.null(name)) magclass::getNames(out) <- name
    return(out)
  }


  ## copy the above function but only return the storage loss part of it. Maybe this is a bit complicated...
  se.prodLoss <- function(prodSe, dataoc, oc2te, sety, enty.input, se.output, te = te_pese2se, name = NULL, storageLoss = storLoss, all_pety = pety) {

    # test if storage loss info exists (realisation dependable)
    if (is.null(storageLoss))
      return(NULL)

    # test if inputs make sense
    if (length(setdiff(enty.input, abind::abind(all_pety, sety))) > 0)
      warning(paste("Input energy enty.input ", setdiff(enty.input, abind::abind(all_pety, sety)), " is not element of pety or sety"))
    if (length(setdiff(se.output, abind::abind(all_pety, sety))) > 0)
      warning(paste("se.output ", setdiff(se.output, abind::abind(all_pety, sety)), " is not element of pety or sety"))
    ## identify all techs with secarrier as a main product
    # sub1_oc2te <- oc2te[(oc2te$all_enty %in% pecarrier) & (oc2te$all_enty1 %in% secarrier) & (oc2te$all_enty2 %in% sety)    & (oc2te$all_te %in% te),]
    ## secondary energy production with secarrier as a main product
    x1 <- dimSums(mselect(prodSe, all_enty = enty.input, all_enty1 = se.output, all_te = te), dim = 3, na.rm = T)
    ## secondary energy production with secarrier as a couple product
    ## identify all oc techs with secarrier as a couple product
    sub_oc2te <- oc2te[(oc2te$all_enty %in% enty.input) & (oc2te$all_enty1 %in% sety)    & (oc2te$all_enty2 %in% se.output) & (oc2te$all_te %in% te), ]
    x2 <- dimSums(prodSe[sub_oc2te] * dataoc[sub_oc2te], dim = 3, na.rm = T)

    ## storage losses
    input.pe2se <- pe2se[(pe2se$all_enty %in% enty.input) & (pe2se$all_enty1 %in% se.output) & (pe2se$all_te %in% te), ]
    if (nrow(input.pe2se) == 0) {
      x3 <- 0
    } else {
      x3 <- dimSums(storageLoss[input.pe2se], dim = 3, na.rm = T)
    }

    out <- (x3)

    if (!is.null(name)) magclass::getNames(out) <- name
    return(out)
  }

  ## reporting should adhere to the following logic:
  ## if a category has more than one subcategory, the subcategories should be reported *explicitly*.

  if (!(is.null(vm_macBase) & is.null(vm_emiMacSector))) {
    ## correction for the reused gas from waste landfills
    MtCH4_2_TWa <- readGDX(gdx, "sm_MtCH4_2_TWa", react = "silent")
    if (is.null(MtCH4_2_TWa)) {
      MtCH4_2_TWa <- 0.001638
    }
    tmp1 <- setNames(
      MtCH4_2_TWa * (vm_macBase[, , "ch4wstl"] - vm_emiMacSector[, , "ch4wstl"]),
      "SE|Gases|Waste (EJ/yr)")
  } else {
    tmp1 <- setNames(new.magpie(cells_and_regions = getRegions(dataoc), years = y, fill = 0),
      "SE|Gases|Waste (EJ/yr)")
  }

  tmp1 <- mbind(tmp1,
    se.prod(prodSe, dataoc, oc2te, sety, abind(pety, sety), sety,              name = "SE (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, sety,                         name = "SE|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, append(pety, "seh2"), "seel",        name = "SE|Electricity (EJ/yr)"),  # seh2 to account for se2se prodution once we add h2 to elec technology
    se.prod(prodSe, dataoc, oc2te, sety, pety, "seel", te = techp,       name = "SE|Electricity|CHP|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seel",                       name = "SE|Electricity|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seel", te = teccs,           name = "SE|Electricity|Biomass|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seel", te = tenoccs,         name = "SE|Electricity|Biomass|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seel", te = "bioigccc",           name = "SE|Electricity|Biomass|IGCCC|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seel", te = "bioigcc",         name = "SE|Electricity|Biomass|IGCC|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seel", te = "biochp",   name = "SE|Electricity|Biomass|CHP|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel",                    name = "SE|Electricity|Coal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = teccs,        name = "SE|Electricity|Coal|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = tenoccs,      name = "SE|Electricity|Coal|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = "igcc",       name = "SE|Electricity|Coal|IGCC|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = "igccc",       name = "SE|Electricity|Coal|IGCCC|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = "pc",         name = "SE|Electricity|Coal|PC|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = "pcc",         name = "SE|Electricity|Coal|PCC|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seel", te = "coalchp", name = "SE|Electricity|Coal|CHP|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel",                     name = "SE|Electricity|Gas (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel", te = teccs,         name = "SE|Electricity|Gas|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel", te = tenoccs,       name = "SE|Electricity|Gas|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel", te = "ngcc",        name = "SE|Electricity|Gas|CC|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel", te = "ngccc",        name = "SE|Electricity|Gas|CCC|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel", te = "ngt",         name = "SE|Electricity|Gas|GT (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seel", te = "gaschp", name = "SE|Electricity|Gas|CHP|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "seh2", "seel",                      name = "SE|Electricity|Hydrogen (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "peoil", "seel",                     name = "SE|Electricity|Oil (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "peoil", "seel", te = tenoccs,       name = "SE|Electricity|Oil|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "peoil", "seel", te = "dot",         name = "SE|Electricity|Oil|DOT (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pety,  "seel", te = terenew_nobio, name = "SE|Electricity|Non-Biomass Renewables (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "peur", "seel",                     name = "SE|Electricity|Nuclear (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegeo", "seel",                     name = "SE|Electricity|Geothermal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pehyd", "seel",                     name = "SE|Electricity|Hydro (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pesol", "seel",                     name = "SE|Electricity|Solar (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pesol", "seel", te = "csp",         name = "SE|Electricity|Solar|CSP (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pesol", "seel", te = "spv",         name = "SE|Electricity|Solar|PV (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pewin", "pesol"), "seel",          name = "SE|Electricity|WindSolar (EJ/yr)"),
    se.prodLoss(prodSe, dataoc, oc2te, sety, "pesol", "seel",                 name = "SE|Electricity|Curtailment|Solar (EJ/yr)"),
    se.prodLoss(prodSe, dataoc, oc2te, sety, "pesol", "seel", te = "csp",     name = "SE|Electricity|Curtailment|Solar|CSP (EJ/yr)"),
    se.prodLoss(prodSe, dataoc, oc2te, sety, "pesol", "seel", te = "spv",     name = "SE|Electricity|Curtailment|Solar|PV (EJ/yr)"),
    se.prodLoss(prodSe, dataoc, oc2te, sety, c("pewin", "pesol"), "seel",      name = "SE|Electricity|Curtailment|WindSolar (EJ/yr)"),
    setNames(se.prod(prodSe, dataoc, oc2te, sety, input_gas, se_Gas) + tmp1[, , "SE|Gases|Waste (EJ/yr)"], "SE|Gases (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Gas,                       name = "SE|Gases|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", se_Gas,                     name = "SE|Gases|Natural Gas (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Gas,                    name = "SE|Gases|Coal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Gas, te = teccs,            name = "SE|Gases|Biomass|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Gas, te = tenoccs,         name = "SE|Gases|Biomass|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Gas, te = teccs,        name = "SE|Gases|Coal|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Gas, te = tenoccs,      name = "SE|Gases|Coal|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "seh2", se_Gas,                      name = "SE|Gases|Hydrogen (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pety, "sehe",                        name = "SE|Heat (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "sehe",                       name = "SE|Heat|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "sehe",                    name = "SE|Heat|Coal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "sehe",                     name = "SE|Heat|Gas (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegeo", "sehe",                     name = "SE|Heat|Geothermal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety,  pety,    "sehe", te = techp,     name = "SE|Heat|CHP (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "sehe", te = "coalchp", name = "SE|Heat|Coal|CHP (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas",  "sehe", te = "gaschp",  name = "SE|Heat|Gas|CHP (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety,  pebio,   "sehe", te = "biochp",  name = "SE|Heat|Biomass|CHP (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c(pety, sety), "seh2",                        name = "SE|Hydrogen (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seh2",                       name = "SE|Hydrogen|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seh2", te = teccs,           name = "SE|Hydrogen|Biomass|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, "seh2", te = tenoccs,         name = "SE|Hydrogen|Biomass|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seh2",                    name = "SE|Hydrogen|Coal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seh2", te = teccs,        name = "SE|Hydrogen|Coal|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", "seh2", te = tenoccs,      name = "SE|Hydrogen|Coal|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seh2",                     name = "SE|Hydrogen|Gas (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seh2", te = teccs,         name = "SE|Hydrogen|Gas|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", "seh2", te = tenoccs,       name = "SE|Hydrogen|Gas|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "seel", "seh2",                      name = "SE|Hydrogen|Electricity (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "seel", "seh2", te = "elh2",         name = "SE|Hydrogen|Electricity|Standard Electrolysis (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "seel", "seh2", te = "elh2VRE",      name = "SE|Hydrogen|Electricity|VRE Storage Electrolysis (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), "seh2",               name = "SE|Hydrogen|Fossil (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), "seh2", te = teccs,    name = "SE|Hydrogen|Fossil|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), "seh2", te = tenoccs,  name = "SE|Hydrogen|Fossil|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, input_liquids, se_Liq,                  name = "SE|Liquids (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq,                          name = "SE|Liquids|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, te = teccs,              name = "SE|Liquids|Biomass|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, te = tenoccs,            name = "SE|Liquids|Biomass|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pebiolc", se_Liq,                name = "SE|Liquids|Biomass|Cellulosic (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pebiolc", se_Liq, teccs,          name = "SE|Liquids|Biomass|Cellulosic|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pebiolc", se_Liq, tenoccs,        name = "SE|Liquids|Biomass|Cellulosic|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pebioil", "pebios"), se_Liq,  name = "SE|Liquids|Biomass|Non-Cellulosic (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pebios", se_Liq,                 name = "SE|Liquids|Biomass|Conventional Ethanol (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pebiolc", se_Liq, "bioethl",        name = "SE|Liquids|Biomass|Biofuel|Ethanol|Cellulosic|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pebios", se_Liq, "bioeths",        name = "SE|Liquids|Biomass|Biofuel|Ethanol|Conventional|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, c("bioftrec", "bioftcrec", "biodiesel"),                                                            name = "SE|Liquids|Biomass|Biofuel (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, c("bioftrec", "biodiesel"), name = "SE|Liquids|Biomass|Biofuel|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, "bioftcrec",    name = "SE|Liquids|Biomass|Biofuel|BioFTRC|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, "bioftrec",        name = "SE|Liquids|Biomass|Biofuel|BioFTR|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Liq, "biodiesel",       name = "SE|Liquids|Biomass|Biofuel|Biodiesel|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Liq,                       name = "SE|Liquids|Coal (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Liq, te = teccs,           name = "SE|Liquids|Coal|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Liq, te = tenoccs,         name = "SE|Liquids|Coal|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", se_Liq,                        name = "SE|Liquids|Gas (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", se_Liq, te = teccs,            name = "SE|Liquids|Gas|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pegas", se_Liq, te = tenoccs,          name = "SE|Liquids|Gas|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), se_Liq,              name = "SE|Liquids|Fossil (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), se_Liq, te = teccs,   name = "SE|Liquids|Fossil|w/ CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), se_Liq, te = tenoccs, name = "SE|Liquids|Fossil|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), se_Liq,               name = "SE|Liquids|Fossil|w/ oil (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, c("pegas", "pecoal", "peoil"), se_Liq, te = tenoccs, name = "SE|Liquids|Fossil|w/ oil|w/o CCS (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "peoil", se_Liq,                       name = "SE|Liquids|Oil (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "seh2", se_Liq,                       name = "SE|Liquids|Hydrogen (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pety, se_Solids,                        name = "SE|Solids (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, "pecoal", se_Solids,                   name = "SE|Solids|Coal (EJ/yr)"),
    # SE|Solids|Biomass is supposed to exclude traditional biomass
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Solids, te = setdiff(pe2se$all_te, "biotr"), name = "SE|Solids|Biomass (EJ/yr)"),
    se.prod(prodSe, dataoc, oc2te, sety, pebio, se_Solids, te = "biotr",         name = "SE|Solids|Traditional Biomass (EJ/yr)")
  )
  if ("sepet" %in% sety) {
    tmp1 <- mbind(tmp1,
      se.prod(prodSe, dataoc, oc2te, sety, pety, "sepet",                      name = "SE|Liquids|sepet (EJ/yr)"),
      se.prod(prodSe, dataoc, oc2te, sety, pety, "sedie",                      name = "SE|Liquids|sedie (EJ/yr)")
    )
  }

# adding compatibility for windoffshore
  if ("windoff" %in% all_te) {
    tmp1 <- mbind(tmp1,
      se.prod(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "wind",     name = "SE|Electricity|Wind|Onshore (EJ/yr)"),
      se.prodLoss(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "wind", name = "SE|Electricity|Curtailment|Wind|Onshore (EJ/yr)"),
      se.prod(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "windoff",     name = "SE|Electricity|Wind|Offshore (EJ/yr)"),
      se.prodLoss(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "windoff", name = "SE|Electricity|Curtailment|Wind|Offshore (EJ/yr)"),
      se.prod(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = c("wind","windoff"),     name = "SE|Electricity|Wind (EJ/yr)"),
      se.prodLoss(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = c("wind","windoff"), name = "SE|Electricity|Curtailment|Wind (EJ/yr)") )
  } else {
    tmp1 <- mbind(tmp1,
      se.prod(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "wind",        name = "SE|Electricity|Wind (EJ/yr)"),
      se.prod(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "wind",        name = "SE|Electricity|Wind|Onshore (EJ/yr)"),
      se.prodLoss(prodSe, dataoc, oc2te, sety, "pewin", "seel", te = "wind",    name = "SE|Electricity|Curtailment|Wind (EJ/yr)")
    )
  }
        
      
  
  if ("segafos" %in% se_Gas) {
    tmp1 <- mbind(tmp1,
      se.prod(prodSe, dataoc, oc2te, sety, pety, "segafos",                     name = "SE|Gases|Fossil (EJ/yr)")
    )
  }

  #    tmp1 <- mbind(tmp1, setNames(se.prod(prodSe,dataoc,oc2te,sety,pebio ,se_Solids, name = NULL)
  #                                 - tmp1[,,"SE|Solids|Traditional Biomass (EJ/yr)"],"SE|Solids|Biomass (EJ/yr)"))



  ### add SE trade variables

  # Note: Quick imlementation for ariadne
  # In the medium-term, this needs to be made consistent with other aggregated SE variables
  # or some SE|Production label needs to be introdued.
  # Preferibly, labeling should be made in line with existing/upcoming project conventions

  if (module2realisation["trade", 2] == "se_trade") {
    vm_Mport <- readGDX(gdx, "vm_Mport", field = "l", restore_zeros = F)[, t, ]
    vm_Xport <- readGDX(gdx, "vm_Xport", field = "l", restore_zeros = F)[, t, ]


    tmp1 <- mbind(tmp1,
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seh2") * pm_conv_TWa_EJ,
        "SE|Hydrogen|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seel") * pm_conv_TWa_EJ,
        "SE|Electricity|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seliqsyn") * pm_conv_TWa_EJ,
        "SE|Liquids|Hydrogen|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "segasyn") * pm_conv_TWa_EJ,
               "SE|Gases|Hydrogen|Net Imports (EJ/yr)"))
  }


  ### FS: SE Demand Reporting

  # FE production
  vm_demFeSector <- readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = F)[, y, ] * pm_conv_TWa_EJ
  # SE demand
  vm_demSe <- readGDX(gdx, "vm_demSe", field = "l", restore_zeros = F)[, y, ] * pm_conv_TWa_EJ
  # conversion efficiency
  pm_eta_conv <- readGDX(gdx, "pm_eta_conv", field = "l", restore_zeros = F)[, y, ]

  tmp1 <- mbind(tmp1,
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel"), dim = 3), "SE|Hydrogen|used for electricity (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel", all_te = "h2turb"), dim = 3), "SE|Hydrogen|used for electricity|normal turbines (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel", all_te = "h2turbVRE"), dim = 3), "SE|Hydrogen|used for electricity|forced VRE turbines (EJ/yr)")
  )


  # if CCU on
  if (module2realisation["CCU", 2] == "on" & "seliqsyn" %in% getNames(vm_demSe, dim = 2)) {
    tmp1 <- mbind(tmp1,
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seliqsyn", all_te = "MeOH"), dim = 3), "SE|Hydrogen|used for synthetic fuels|liquids (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "segasyn", all_te = "h22ch4"), dim = 3), "SE|Hydrogen|used for synthetic fuels|gases (EJ/yr)")
    )
  }

  # SE electricity use

  # SE electricity use

  ### calculation of electricity use for own consumption of energy system
  vm_prodFe <- readGDX(gdx, "vm_prodFe", field = "l", restore_zeros = F)
  vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = F)

  # filter for coupled production coefficents which consume seel
  # (have all_enty2=seel and are negative)
  teprodCoupleSeel <- getNames(mselect(dataoc_tmp, all_enty2 = "seel"), dim = 3)
  CoeffOwnConsSeel <- dataoc_tmp[, , teprodCoupleSeel]
  CoeffOwnConsSeel[CoeffOwnConsSeel > 0] <- 0
  CoeffOwnConsSeel_woCCS <- CoeffOwnConsSeel[, , "ccsinje", invert = T]

  # FE and SE production that has own consumption of electricity
  # calculate prodSe back to TWa (was in EJ before), but prod couple coefficient is defined in TWa(input)/Twa(output)
  prodOwnCons <- mbind(vm_prodFe, prodSe / pm_conv_TWa_EJ)[, , getNames(CoeffOwnConsSeel_woCCS, dim = 3)]

  tmp1 <- mbind(tmp1, setNames(
    -pm_conv_TWa_EJ *
      (dimSums(CoeffOwnConsSeel_woCCS * prodOwnCons[, , getNames(CoeffOwnConsSeel_woCCS, dim = 3)], dim = 3, na.rm = T) +
        dimSums(CoeffOwnConsSeel[, , "ccsinje"] * vm_co2CCS[, , "ccsinje"], dim = 3,  na.rm = T)),
    "SE|Electricity|used for own consumption of energy system (EJ/yr)"))


  # electricity for decentral ground heat pumps
  tmp1 <- mbind(tmp1, setNames(
    -pm_conv_TWa_EJ *
      (dimSums(CoeffOwnConsSeel_woCCS[, , "geohe"] * prodOwnCons[, , "geohe"], dim = 3)),
    "SE|Electricity|used for centralized geothermal heat pumps (EJ/yr)"))


  # share of electrolysis H2 in total H2
  p_shareElec_H2 <- collapseNames(tmp1[, , "SE|Hydrogen|Electricity (EJ/yr)"] / tmp1[, , "SE|Hydrogen (EJ/yr)"])
  p_shareElec_H2[is.na(p_shareElec_H2)] <- 0


  # share of domestically produced H2 (only not 1 if se trade module on and hydrogen can be imported/exported)
  if (module2realisation["trade", 2] == "se_trade") {
    p_share_H2DomProd <-  collapseNames(tmp1[, , "SE|Hydrogen (EJ/yr)"] / (tmp1[, , "SE|Hydrogen|Net Imports (EJ/yr)"] + tmp1[, , "SE|Hydrogen (EJ/yr)"]))
  } else {
    p_share_H2DomProd <- tmp1[, , "SE|Hydrogen (EJ/yr)"]
    p_share_H2DomProd[] <- 1
  }




  tmp1 <- mbind(tmp1,
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "build"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Electricity|used in Buildings (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "indst"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Electricity|used in Industry (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feelt", emi_sectors = "trans"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdelt"),
    "SE|Electricity|used in Transport (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "CDR"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Electricity|used for CDR (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2"), dim = 3),
      "SE|Electricity|used for H2 (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2", all_te = "elh2"), dim = 3),
      "SE|Electricity|used for H2|Standard Electrolysis (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2", all_te = "elh2VRE"), dim = 3),
      "SE|Electricity|used for H2|VRE Storage (EJ/yr)")
  )

  # calculate electricity going into domestic (!) H2 production for direct H2 use (FE Hydrogen)
  tmp1 <- mbind(tmp1,
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("feh2s", "feh2t")), dim = 3) * p_share_H2DomProd *
      p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
    "SE|Electricity|used for H2|direct FE H2 (EJ/yr)"))

  # electricity used for domestic (!) synfuel production
  if (module2realisation["CCU", 2] == "on" & "seliqsyn" %in% getNames(vm_demSe, dim = 2)) {

    tmp1 <- mbind(tmp1,
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Electricity|used for H2|for synthetic fuels|liquids (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("segasyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Electricity|used for H2|for synthetic fuels|gases (EJ/yr)"))

  }



  # transmission losses from se2fe conversion of electricity
  tmp1 <- mbind(tmp1,
    setNames(dimSums(vm_demSe[, , "tdels"] * (1 - pm_eta_conv[, , "tdels"]), dim = 3),
      "SE|Electricity|Transmission Losses (EJ/yr)"))



  # add global values
  out <- mbind(tmp1, dimSums(tmp1, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))

  return(out)
}
