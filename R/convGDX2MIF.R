#' Read in GDX and write *.mif reporting
#'
#' Read in all information from GDX file and create the *.mif reporting.
#'
#' @md
#' @param gdx gdx file path.
#' @param gdx_ref Reference gdx file path, used for fixing the prices to this
#'     scenario for all time steps before `cm_startyear`.
#' @param file Name of the .mif file which will be written.  If no name is
#'     provided a magpie object containing all the reporting information is
#'     returned.
#' @param scenario Scenario name that is used in the *.mif reporting.  Defaults
#'     to `'default'`.
#' @param t Temporal resolution of the reporting, defaults to
#'     `t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)`.
#' @param gdx_refpolicycost Reference gdx file path for policy costs.
#' @param verbose Report on reporting functions being called.
#'
#' @author Lavinia Baumstark
#'
#' @examples
#' \dontrun{
#'     convGDX2MIF(gdx, gdx_refpolicycost, file = "REMIND_generic_default.csv",
#'                 scenario = "default")
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom magclass mbind write.report
#' @importFrom methods formalArgs getFunction

#' @export
convGDX2MIF <- function(gdx, gdx_ref = NULL, file = NULL, scenario = "default",
                        t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130,
                              2150),
                        gdx_refpolicycost = gdx_ref,
                        verbose = TRUE) {

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
  # parameters as a list, in which arguments can be named if their name inside
  # convGDX2MIF differs from the argument name of the reporting function, e.g.
  #   list('reportPolicyCosts',
  #        'gdx_ref' = gdx_refpolicycost,
  #        'check_GDPscen' = TRUE)
  # Arguments  must exist in the scope of this function (i.e. must be accessible
  # by code run inside this function).
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
    list('reportPrices', gdx_ref),
    'reportCosts',
    'reportTax',
    'reportCrossVariables',
    list('reportPolicyCosts',
         # convGDX2MIF() has an additional argument for the gdx used in
         # reportPolicyCosts(), gdx_refpolicycost, but reportPolicyCosts() uses
         # just gdx_ref as an argument name, so this parameter look a bit weird.
         'gdx_ref' = gdx_refpolicycost,
         'check_GDPscen' = TRUE),
    'reportSDPVariables'
  )

  # Default arguments passed to all reporting functions, provided they accept
  # them.
  default_reporting_arguments <- c('gdx', 'output', 'regionSubsetList', 't')

  ## execution ----
  .get_reporting_function <- function(reporting_function_name) {
    # Get the reporting function by name from the remind2 package, print an
    # expressive error message should that fail.
    tryCatch(
      getFunction(reporting[[1]], where = getNamespace('remind2')),
      error = function(e) {
        known_error_message <- paste0('^no function .', reporting[[1]],
                                      '. found$')
        if (grepl(known_error_message, e[['message']])) {
          stop('Function `', reporting[[1]],
               '()` not defined in package remind2.')
        }
        else {
          stop(e)
        }
      })
  }

  .get_reporting_function_arguments <- function(reporting_function,
                                                declared_arguments,
                                                default_reporting_arguments,
                                                env) {
    # Create a named list of function arguments from default arguments and
    # declared arguments.  Should the reporting function expect an argument that
    # is not in <default_reporting_arguments>, that is not declared in
    # the reporting, and for which it has no default value, R will throw an
    # error.
    reporting_function_arguments <- intersect(
      setdiff(formalArgs(reporting_function), names(declared_arguments)),
      default_reporting_arguments)

    reporting_function_arguments <- sapply(
      X = reporting_function_arguments,
      FUN = get, envir = convGDX2MIF_environment, inherits = FALSE,
      simplify = FALSE, USE.NAMES = TRUE)

    reporting_function_arguments <- c(
      reporting_function_arguments,
      declared_arguments)

    return(reporting_function_arguments)
  }

  output <- NULL
  convGDX2MIF_environment <- environment()

  for (reporting in reportings) {
    reporting_function_name <- reporting[[1]]
    declared_arguments <- reporting[-1]

    reporting_function <- .get_reporting_function(reporting_function_name)
    reporting_function_arguments <- .get_reporting_function_arguments(
      reporting_function, declared_arguments, default_reporting_arguments,
      convGDX2MIF_environment)

    # Call the reporting function, catching any errors it might throw.  Warn the
    # user about errors and carry on, or join the resulting data.  Filter the
    # reporting return values to <t> time steps (some functions report more).
    if (verbose)
      message('running ', reporting_function_name, '...')

    tryCatch(
      expr = { output_reporting <- do.call(reporting_function,
                                           args = reporting_function_arguments)
      output <- mbind(output, output_reporting[,t,])
      },
      error = function(e) {
        message('Function ', reporting_function_name,
                '() failed and is skipped\n',
                '  Error message: ', e[['message']])
      }
    )
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
