#' Report energy investments
#'
#' Report energy investments
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param gdx_ref a GDX object as created by readGDX, or the path to a gdx of the reference run.
#' It is used to guarantee consistency before 'cm_startyear' for investment variables
#' using time averaging.
#'
#' @return magclass object
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportEnergyInvestment(gdx)
#' }
#' @export
#'
reportEnergyInvestment <- function(gdx,
                                   regionSubsetList = NULL,
                                   t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                                   gdx_ref = NULL) {
  # Load in energy investment cost variables, (see equation q_costInv)
  v_costInv <- gdx::readGDX(gdx, "v_costInv", field = "l")
  vm_costInvTeDir <- gdx::readGDX(gdx, "vm_costInvTeDir", field = "l")
  vm_costInvTeAdj <- gdx::readGDX(gdx, "vm_costInvTeAdj", field = "l")
  vm_costAddTeInv <- gdx::readGDX(gdx, "vm_costAddTeInv", field = "l")
  vm_costCESMkup <- gdx::readGDX(gdx, "vm_costCESMkup", field = "l")

  # Apply 'modifyInvestmentVariables' to shift from the model-internal time coverage (deltacap and investment
  # variables for step t represent the average of the years from t-4years to t) to the general convention for
  # the reporting template (all variables represent the average of the years from t-2.5years to t+2.5years).
  # This function requires the variable to be defined over ttot.
  ttot <- gdx::readGDX(gdx, "ttot") |> as.numeric()
  cm_startyear <- gdx::readGDX(gdx, "cm_startyear") |> as.integer()

  v_costInv <- modifyInvestmentVariables(v_costInv[, ttot, ], ref = gdx_ref, startYear = cm_startyear)
  vm_costInvTeDir <- modifyInvestmentVariables(vm_costInvTeDir[, ttot, ], ref = gdx_ref, startYear = cm_startyear)
  vm_costInvTeAdj <- modifyInvestmentVariables(vm_costInvTeAdj[, ttot, ], ref = gdx_ref, startYear = cm_startyear)
  vm_costAddTeInv <- modifyInvestmentVariables(vm_costAddTeInv[, ttot, ], ref = gdx_ref, startYear = cm_startyear)
  vm_costCESMkup <- modifyInvestmentVariables(vm_costCESMkup[, ttot, ], ref = gdx_ref, startYear = cm_startyear)

  # Load in sets
  en2en <- gdx::readGDX(gdx, "en2en") |> dplyr::rename("en_in" = "all_enty", "en_out" = "all_enty1", "te" = "all_te")
  teStor <- gdx::readGDX(gdx, "teStor") |> as.character()
  teGrid <- gdx::readGDX(gdx, "teGrid") |> as.character()
  teNoTransform <- gdx::readGDX(gdx, "teNoTransform") |> as.character()
  teCCS <- gdx::readGDX(gdx, "teCCS") |> as.character()
  peFos <- gdx::readGDX(gdx, "peFos") |> as.character()
  peBio <- gdx::readGDX(gdx, "peBio") |> as.character()
  peRe <- gdx::readGDX(gdx, "peRe") |> as.character()
  entyFe <- gdx::readGDX(gdx, "entyFe") |> as.character()
  entyPe <- gdx::readGDX(gdx, "entyPe") |> as.character()
  entySe <- gdx::readGDX(gdx, "entySe") |> as.character()
  sector2te_addTDCost <- gdx::readGDX(gdx, "sector2te_addTDCost") |>
    tidyr::unite("x", c("all_te", "emi_sectors"), sep = ".") |>
    dplyr::pull()
  ppfen_CESMkup <- gdx::readGDX(gdx, "ppfen_CESMkup") |> as.character()
  tePrc <- gdx::readGDX(gdx, "tePrc") |> as.character()

  # Get the energy conversion and non-conversion techs
  te_used <- c(en2en$te, teNoTransform)

  # Combine the direct investment- and adjustment costs, filter by t and te_used, and convert to billion $.
  # These are the supply-side energy investments.
  inv <- vm_costInvTeDir + vm_costInvTeAdj
  inv <- inv[, t, te_used] * 1e3

  # Combine the additional transmission and distribution and CES markup costs. These are the demand-side investments.
  ## (vm_costCESMkup represents demand-side technology cost of end-use transformation)
  ## (vm_costAddTeInv represent additional sector-specific investment cost of demand-side transformation)
  dem_costs <- dimSums(vm_costAddTeInv[, t, sector2te_addTDCost]) + dimSums(vm_costCESMkup[, t, ppfen_CESMkup])

  # Total energy investments split into supply and demand
  tmp <- setNames(v_costInv[, t, ] * 1e3, "Energy Investments (billion US$2017/yr)")
  tmp <- mbind(tmp, setNames(dimSums(inv), "Energy Investments|+|Supply (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(dem_costs * 1e3, "Energy Investments|+|Demand (billion US$2017/yr)"))

  # Total energy investments split into fossil and non-fossil
  te_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos)$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_fossil]), "Energy Investments|++|Fossil (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(
    tmp[, , "Energy Investments (billion US$2017/yr)"] - tmp[, , "Energy Investments|++|Fossil (billion US$2017/yr)"],
    "Energy Investments|++|Non-Fossil (billion US$2017/yr)"
  ))

  # In the following the supply side investments are split into SE carriers
  # Investments into electricity split by generation vs grid&storage
  te_el_generation <- dplyr::filter(en2en, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_generation]),
    "Energy Investments|Supply|Electricity|+|Generation (billion US$2017/yr)"
  ))
  ## We add transmission and distribution techs to grid
  te_grid_td <- c(teGrid, "tdels", "tdelt")
  te_grid_storage <- c(te_grid_td, teStor)
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_grid_storage]),
    "Energy Investments|Supply|Electricity|+|Grid&Storage (billion US$2017/yr)"
  ))

  ## Total electricity investments
  te_el <- c(te_el_generation, te_grid_storage)
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el]),
    "Energy Investments|Supply|+|Electricity (billion US$2017/yr)"
  ))

  # Investments into electricity generation split by primary energy
  ## Coal
  te_coal <- dplyr::filter(en2en, .data$en_in == "pecoal", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_coal]),
    "Energy Investments|Supply|Electricity|Generation|+|Coal (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_coal[te_coal %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Coal|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_coal[!te_coal %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Coal|+|w/o CC (billion US$2017/yr)"
  ))

  ## Gas
  te_gas <- dplyr::filter(en2en, .data$en_in == "pegas", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gas]),
    "Energy Investments|Supply|Electricity|Generation|+|Gas (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gas[te_gas %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Gas|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gas[!te_gas %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Gas|+|w/o CC (billion US$2017/yr)"
  ))

  ## Oil
  te_oil <- dplyr::filter(en2en, .data$en_in == "peoil", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_oil]),
    "Energy Investments|Supply|Electricity|Generation|+|Oil (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_oil[te_oil %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Oil|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_oil[!te_oil %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Oil|+|w/o CC (billion US$2017/yr)"
  ))

  ## Biomass
  te_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_bio]),
    "Energy Investments|Supply|Electricity|Generation|+|Biomass (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_bio[te_bio %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Biomass|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_bio[!te_bio %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Biomass|+|w/o CC (billion US$2017/yr)"
  ))

  ## Nuclear
  te_nuclear <- dplyr::filter(en2en, .data$en_in == "peur", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nuclear]),
    "Energy Investments|Supply|Electricity|Generation|+|Nuclear (billion US$2017/yr)"
  ))

  ## Solar
  te_solar <- dplyr::filter(en2en, .data$en_in == "pesol", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_solar]),
    "Energy Investments|Supply|Electricity|Generation|+|Solar (billion US$2017/yr)"
  ))

  ## Wind
  te_wind <- dplyr::filter(en2en, .data$en_in == "pewin", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_wind]),
    "Energy Investments|Supply|Electricity|Generation|+|Wind (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "windon"]),
    "Energy Investments|Supply|Electricity|Generation|Wind|+|Onshore (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "windoff"]),
    "Energy Investments|Supply|Electricity|Generation|Wind|+|Offshore (billion US$2017/yr)"
  ))

  ## Hydro
  te_hydro <- dplyr::filter(en2en, .data$en_in == "pehyd", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_hydro]),
    "Energy Investments|Supply|Electricity|Generation|+|Hydro (billion US$2017/yr)"
  ))

  ## Geothermal
  te_geo <- dplyr::filter(en2en, .data$en_in == "pegeo", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_geo]),
    "Energy Investments|Supply|Electricity|Generation|+|Geothermal (billion US$2017/yr)"
  ))

  ## Hydrogen
  te_hydrogen <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_hydrogen]),
    "Energy Investments|Supply|Electricity|Generation|+|Hydrogen (billion US$2017/yr)"
  ))

  # Investments into electricity generation split by fossil, non-fossil, bio-renew and non-bio-renew pes.
  ## Fossil (coal, gas and oil)
  te_el_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_fossil]),
    "Energy Investments|Supply|Electricity|Generation|++|Fossil (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_fossil[te_el_fossil %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Fossil|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_fossil[!te_el_fossil %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Fossil|+|w/o CC (billion US$2017/yr)"
  ))
  ## Non-Fossil
  te_nonfossil <- dplyr::filter(en2en, !.data$en_in %in% peFos, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonfossil]),
    "Energy Investments|Supply|Electricity|Generation|++|Non-Fossil (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonfossil[te_nonfossil %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Non-Fossil|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonfossil[!te_nonfossil %in% teCCS]]),
    "Energy Investments|Supply|Electricity|Generation|Non-Fossil|+|w/o CC (billion US$2017/yr)"
  ))
  ## Bio-renewables
  te_biorenew <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_biorenew]),
    "Energy Investments|Supply|Electricity|Generation|Non-Fossil|++|Bio Re (billion US$2017/yr)"
  ))
  ## Non-bio-renewables and nuclear
  pe_nonbiorenew <- peRe[!peRe %in% peBio]
  te_nonbiorenew <- dplyr::filter(en2en, .data$en_in %in% pe_nonbiorenew, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonbiorenew]),
    "Energy Investments|Supply|Electricity|Generation|Non-Fossil|++|Non-Bio Re (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nuclear]),
    "Energy Investments|Supply|Electricity|Generation|Non-Fossil|++|Nuclear (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_hydrogen]),
    "Energy Investments|Supply|Electricity|Generation|Non-Fossil|++|Hydrogen (billion US$2017/yr)"
  ))

  # Investments into electricity grid and storage split by specific types
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , teStor]),
    "Energy Investments|Supply|Electricity|Grid&Storage|+|Storage (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_grid_td]),
    "Energy Investments|Supply|Electricity|Grid&Storage|+|Grid (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , teGrid]),
    "Energy Investments|Supply|Electricity|Grid&Storage|Grid|+|VRE support (billion US$2017/yr)"
  ))
  ## Split out the amount of "normal" grid, the additional grid/charger investments due to electric vehicles,
  ## and the additional grid investments needed for better pooling of VRE. For BEVs, the assumption is to only
  ## take the share of tdelt costs that are higher than the tdels costs
  pm_data <- gdx::readGDX(gdx, "pm_data")[, , c("inco0.tdelt", "inco0.tdels")]
  cr <- pm_data[, , "inco0.tdelt"] / pm_data[, , "inco0.tdels"]
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "tdelt"]) * (cr - 1) / cr,
    "Energy Investments|Supply|Electricity|Grid&Storage|Grid|+|BEV Chargers (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    (dimSums(inv[, , "tdelt"]) / cr + dimSums(inv[, , "tdels"])),
    "Energy Investments|Supply|Electricity|Grid&Storage|Grid|+|Normal (billion US$2017/yr)"
  ))

  # Investments into hydrogen
  te_h2 <- dplyr::filter(en2en, (.data$en_in == "seh2" & .data$en_out %in% entyFe) | .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_h2]), "Energy Investments|Supply|+|Hydrogen (billion US$2017/yr)"))

  te_h2_pe <- dplyr::filter(en2en, .data$en_in %in% entyPe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_pe]),
    "Energy Investments|Supply|Hydrogen|++|PE (billion US$2017/yr)"
  ))

  te_h2_se <- dplyr::filter(en2en, .data$en_in %in% entySe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_se]),
    "Energy Investments|Supply|Hydrogen|++|se2se (billion US$2017/yr)"
  ))

  te_h2_fe <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% entyFe)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_fe]),
    "Energy Investments|Supply|Hydrogen|++|se2fe (billion US$2017/yr)"
  ))

  te_h2_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_fossil]),
    "Energy Investments|Supply|Hydrogen|+|Fossil (billion US$2017/yr)"
  ))

  te_h2_re <- dplyr::filter(en2en, .data$en_in %in% peRe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_re]),
    "Energy Investments|Supply|Hydrogen|+|RE (billion US$2017/yr)"
  ))

  te_h2_se <- dplyr::filter(en2en, .data$en_in %in% entySe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_se]),
    "Energy Investments|Supply|Hydrogen|+|Electrolysis (billion US$2017/yr)"
  ))

  te_h2_fe <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% entyFe)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_fe]),
    "Energy Investments|Supply|Hydrogen|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  te_h2_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_bio]),
    "Energy Investments|Supply|Hydrogen|Bio (billion US$2017/yr)"
  ))

  # Investments into gases
  se_gas <- c("segafos", "segabio", "segasyn")
  te_gases <- dplyr::filter(en2en, .data$en_in %in% se_gas | .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_gases]), "Energy Investments|Supply|+|Gases (billion US$2017/yr)"))

  te_gases_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_fossil]),
    "Energy Investments|Supply|Gases|+|Fossil (billion US$2017/yr)"
  ))

  te_gases_h2 <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_h2]),
    "Energy Investments|Supply|Gases|+|Hydrogen (billion US$2017/yr)"
  ))

  te_gases_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_bio]),
    "Energy Investments|Supply|Gases|+|Biomass (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_bio[te_gases_bio %in% teCCS]]),
    "Energy Investments|Supply|Gases|Biomass|+|w/ CC (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_bio[!te_gases_bio %in% teCCS]]),
    "Energy Investments|Supply|Gases|Biomass|+|w/o CC (billion US$2017/yr)"
  ))

  te_gases_td <- dplyr::filter(en2en, .data$en_in %in% se_gas, .data$en_out %in% entyFe)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_td]),
    "Energy Investments|Supply|Gases|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  # Investments into heat
  te_heat <- dplyr::filter(en2en, .data$en_in == "sehe" | .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_heat]), "Energy Investments|Supply|+|Heat (billion US$2017/yr)"))

  te_heat_pump <- dplyr::filter(en2en, .data$en_in == "pegeo", .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_pump]),
    "Energy Investments|Supply|Heat|+|Heat Pump (billion US$2017/yr)"
  ))

  te_heat_gas <- dplyr::filter(en2en, .data$en_in == "pegas", .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_gas]),
    "Energy Investments|Supply|Heat|+|Gas (billion US$2017/yr)"
  ))

  te_heat_coal <- dplyr::filter(en2en, .data$en_in == "pecoal", .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_coal]),
    "Energy Investments|Supply|Heat|+|Coal (billion US$2017/yr)"
  ))

  te_heat_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_fossil]),
    "Energy Investments|Supply|Heat|Fossil (billion US$2017/yr)"
  ))

  te_heat_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_bio]),
    "Energy Investments|Supply|Heat|+|Biomass (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "tdhes"]),
    "Energy Investments|Supply|Heat|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  # Investments into liquids
  se_liq <- c("seliqfos", "seliqbio", "seliqsyn")
  te_liq <- dplyr::filter(en2en, .data$en_in %in% se_liq | .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq]),
    "Energy Investments|Supply|+|Liquids (billion US$2017/yr)"
  ))

  te_liq_fos <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_fos]),
    "Energy Investments|Supply|Liquids|+|Fossil (billion US$2017/yr)"
  ))

  te_liq_or <- dplyr::filter(en2en, .data$en_in == "peoil", .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_or]),
    "Energy Investments|Supply|Liquids|Fossil|+|Oil Ref (billion US$2017/yr)"
  ))

  te_liq_fosno <- dplyr::filter(en2en, .data$en_in %in% c("pecoal", "pegas"), .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_fosno]),
    "Energy Investments|Supply|Liquids|Fossil|+|w/o oil (billion US$2017/yr)"
  ))

  te_liq_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_bio]),
    "Energy Investments|Supply|Liquids|+|Bio (billion US$2017/yr)"
  ))

  te_liq_h2 <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_h2]),
    "Energy Investments|Supply|Liquids|+|Hydrogen (billion US$2017/yr)"
  ))

  te_liq_td <- dplyr::filter(en2en, .data$en_in %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_td]),
    "Energy Investments|Supply|Liquids|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  # Investments into solids
  se_sol <- c("sesofos", "sesobio")
  te_sol <- dplyr::filter(en2en, .data$en_in %in% se_sol | .data$en_out %in% se_sol)$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_sol]), "Energy Investments|Supply|+|Solids (billion US$2017/yr)"))

  # Investments into biochar
  te_biochar <- dplyr::filter(en2en, .data$en_out == "sebiochar")$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_biochar]), "Energy Investments|Supply|+|Biochar (billion US$2017/yr)"))

  # Investments into DAC
  tmp <- mbind(tmp, setNames(dimSums(inv[, , "dac"]), "Energy Investments|Supply|+|DAC (billion US$2017/yr)"))

  # Investments into CCS Trans and Stor
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "ccsinje"]),
    "Energy Investments|Supply|+|CO2 Trans&Stor (billion US$2017/yr)"
  ))

  # Investments into Steel
  tmp <- mbind(tmp, setNames(dimSums(inv[, , tePrc]), "Energy Investments|Supply|+|Steel (billion US$2017/yr)"))

  # Add non-electricity aggregate
  tmp <- mbind(tmp, setNames(
    tmp[, , "Energy Investments|+|Supply (billion US$2017/yr)"] -
      tmp[, , "Energy Investments|Supply|+|Electricity (billion US$2017/yr)"],
    "Energy Investments|Supply|Non-Electricity (billion US$2017/yr)"
  ))

  # Add global values
  tmp <- mbind(tmp, dimSums(tmp, dim = 1))
  # Add other region aggregations
  if (!is.null(regionSubsetList)) {
    tmp <- mbind(tmp, calc_regionSubset_sums(tmp, regionSubsetList))
  }

  getSets(tmp)[3] <- "variable"
  tmp
}
