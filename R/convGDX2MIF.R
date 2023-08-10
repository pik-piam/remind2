#' Read in GDX and write *.mif reporting
#'
#' Read in all information from GDX file and create
#' the *.mif reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param gdx_ref reference-gdx for < cm_startyear, used for fixing the prices to this scenario
#' @param file name of the mif file which will be written, if no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param gdx_refpolicycost reference-gdx for policy costs, a GDX as created by readGDX, or the file name of a gdx
#' @author Lavinia Baumstark
#' @examples
#'
#' \dontrun{convGDX2MIF(gdx,gdx_refpolicycost,file="REMIND_generic_default.csv",scenario="default")}
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mbind write.report
#' @importFrom methods formalArgs getFunction

convGDX2MIF <- function(gdx, gdx_ref = NULL, file = NULL, scenario = "default",
                        t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                        gdx_refpolicycost = gdx_ref) {

  # Define region subsets
  regionSubsetList <- toolRegionSubsets(gdx)

  # ADD EU-27 region aggregation if possible
  if ("EUR" %in% names(regionSubsetList)) {
    regionSubsetList <- c(regionSubsetList,
                          list("EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE",
                                          "FRA", "DEU", "ESW")))
  }

  # reporting functions ----

  ## setup ----
  # List all reporting functions to be called by name in the order they should
  # be called.  If the function needs parameters not included in the default
  # reporting arguments (see below), add the function name and the names of the
  # parameters as a vector, e.g. c('reportPrices', 'gdx_ref').  The parameters
  # must exist in the scope of this function (i.e. must be accessible by code
  # run inside this function).
  reportings <- list(
    'reportMacroEconomy',
    'reportTrade',
    'reportPE',
    'reportSE',
    'reportFE',
    'reportExtraction',
    'reportCapacity',
    'reportCapitalStock',
    'reportEnergyInvestment',
    'reportEmiAirPol',
    'reportEmi',
    'reportTechnology',
    c('reportPrices', 'gdx_ref'),
    'reportCosts',
    'reportTax',
    'reportCrossVariables'
  )

  # Default arguments passed to all reporting functions, provided they accept
  # them.
  default_reporting_arguments <- c('gdx', 'output', 'regionSubsetList', 't')

  ## execution ----
  output <- NULL

  for (reporting in reportings) {
    # Get the reporting function by name from the remind2 package, print an
    # expressive error message should that fail.
    reporting_function <- tryCatch(
      getFunction(reporting[[1]], where = getNamespace('remind2')),
      error = function(e) {
        known_error_message <- paste0('^no function .', reporting[[1]],
                                      '. found$')
        if (   inherits(e, 'error')
            && grepl(known_error_message, e[['message']])) {
          stop('Function foo() not defined in package remind2.')
        }
        else {
          stop(e)
        }
      })

    # Create a named list of function arguments from default arguments and
    # declared arguments.  Should the reporting function expect an argument that
    # is not in <default_reporting_arguments>, that is not declared in
    # <reportings>, and for which it has no default value, R will throw an
    #  error.
    convGDX2MIF_environment <- environment()

    reporting_function_arguments <- intersect(formalArgs(reporting_function),
                                              c(default_reporting_arguments,
                                                reporting[-1]))
    reporting_function_arguments <- sapply(
      X = reporting_function_arguments,
      FUN = get, envir = convGDX2MIF_environment, inherits = FALSE,
      simplify = FALSE, USE.NAMES = TRUE)

    # Call the reporting function, catching any errors it might throw.  Warn the
    # user about errors and carry on, or join the resulting data.  Filter the
    # reporting return values to <t> time steps (some functions report more).
    output_reporting <- try(do.call(reporting_function,
                                    args = reporting_function_arguments))

    if (inherits(output_reporting, 'try-error')) {
      message('Function ', reporting[[1]], '() failed and is skipped\n',
              '  Error message: ',
              attr(output_reporting, 'condition')[['message']])
    }
    else {
      output <- mbind(output, output_reporting[,t,])
    }
  }

  # additional reporting ----
  ## Report policy costs, if possible and sensible ----
  if(!is.null(gdx_refpolicycost)) {
    if (file.exists(gdx_refpolicycost)) {
      gdp_scen <- try(readGDX(gdx,"cm_GDPscen",react ="error"),silent=T)
      gdp_scen_ref <- try(readGDX(gdx_refpolicycost,"cm_GDPscen",react = "error"),silent=T)
      if(!inherits(gdp_scen,"try-error") && !inherits(gdp_scen_ref,"try-error")){
        if(gdp_scen[1]==gdp_scen_ref[1]){
          message("running reportPolicyCosts, comparing to ", basename(dirname(gdx_refpolicycost)), "/", basename(gdx_refpolicycost), "...")
          output <- mbind(output,reportPolicyCosts(gdx,gdx_refpolicycost,regionSubsetList,t)[,t,])
        } else {
          warning(paste0("The GDP scenario differs from that of the reference run. Did not execute 'reportPolicyCosts'! If a policy costs reporting is desired, please use the 'policyCosts' output.R script."))
        }
      } else {
        warning(paste0("A comparison of the GDP scenarios between this run and its reference run wasn't possible (old remind version). Therefore to avoid reporting unsensible policy costs, 'reportPolicyCosts' was not executed. If a policy costs reporting is required, please use the  'policyCosts' output.R script."))
      }
    } else {
      warning(paste0("File ",gdx_refpolicycost," not found. Did not execute 'reportPolicyCosts'! If a policy costs reporting is desired, please use the   'policyCosts' output.R script."))
    }
  }

  ## reporting of SDP variables ----
  message("running reportSDPVariables...")
  tmp <- try(reportSDPVariables(gdx,output,t))  # test whether reportSDPVariables works
  if (!inherits(tmp, "try-error")) {
    if (!is.null(tmp))
      output <- tmp
  }
  else {
    message("function reportSDPVariables does not work and is skipped")
  }

  # create output ----
  # Add dimension names "scenario.model.variable"
  getSets(output)[3] <- "variable"
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "REMIND")
  output <- add_dimension(output, dim = 3.1, add = "scenario", nm = scenario)

  # either write the *.mif or return the magpie object
  if (!is.null(file)) {
    write.report(output, file = file, ndigit = 7)
    # write same reporting without "+" or "++" in variable names
    deletePlus(file, writemif = TRUE)
  }
  else {
    return(output)
  }
}
