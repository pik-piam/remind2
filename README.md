# The REMIND R package (2nd generation)

R package **remind2**, version **1.86.12**

[![CRAN status](https://www.r-pkg.org/badges/version/remind2)](https://cran.r-project.org/package=remind2)  [![R build status](https://github.com/pik-piam/remind2/workflows/check/badge.svg)](https://github.com/pik-piam/remind2/actions) [![codecov](https://codecov.io/gh/pik-piam/remind2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/remind2) [![r-universe](https://pik-piam.r-universe.dev/badges/remind2)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Contains functions for creating the reporting of the REMIND model in form of a .mif-file. It includes the function ``convGDX2MIF()`` which creates the main REMIND output contained in the mif-file based on the fulldata.gdx file of a REMIND run. The calculation of the REMIND variables as reported in the mif-file is separated into different functions per variable category (e.g. ``SE|...`` variables are calculated in reportSE.R, ``Emi|...`` variables in reportEmi.R etc.). 



## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remind2")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("compareScenarios2") # compareScenarios2
vignette("remind_summary")    # Adding plots to the REMIND_summary.pdf
```

## Development



When developing the remind2 library, please test your changes on a recent ``fulldata.gdx`` file from REMIND by running the ``convGDX2MIF()`` function. For example:

```r
gdx <- "fulldata.gdx"
testReport <- convGDX2MIF(gdx)
```

Make sure the ``fulldata.gdx`` file is suitable for testing the change you did. For example, check that the relevant GAMS variables and parameters are available and non-zero in the GDX you are testing. Different REMIND runs come with different module realization and switches where certain sets, parameters or variables are not existing such that your change may not be tested by an unsuitable GDX. Moreover, when adapating or adding variables with "+" notation, please run a summation check on a suitable GDX by running the ``checkIntegrity()`` found [here](https://github.com/pik-piam/remind2/blob/1a80cc607db663a211f98cabb3c7408cd08bf713/tests/testthat/test-convGDX2mif.R#L31) on your reporting output:

```r
checkIntegrity(testReport)
```

If your change involves reading in a parameter, variable or set from the GDX-file that is not yet merged to the [main REMIND repository](https://github.com/remindmodel/remind) or that is not declared in the default runs of REMIND, please use conditional statements that check whether these parameters exist. Otherwise, the reporting will break for other users with other GDX files. 


## REMIND Variable Naming

There are a couple of naming conventions for variables in the REMIND mif-file:

* variables typically follow the structure ``Variable Type|...|Sector1|Sector2|…|Energy Carrier Output|Energy Carrier Origin`` (Example: ``FE|Industry|Liquids|Biomass`` contains liquid fuel used as final energy in industry that is produced from biomass)
* variables including one or several "+" in the name are subject to an automated summation check by ``test-convGDX2mif.R`` when building the library (for the naming convention of variables with "+", see below)  
* modifications of standard variables like ``|w/ Bunkers|``, ``|before taxes|`` or ``|gross|`` should be placed at the highest relevant aggregation level (examples: ``Emi|CO2|Gross|Energy|Supply|+|Liquids``, ``FE|w/ Bunkers|+|Transport``)
* variables which should not be used for papers or external projects and which were added solely for diagnostic or debugging purpose should start with ``Internal|...``

Several variable types are structured in a variable tree using "+" notation their name. This means that it is checked that a group of variables sums up to an aggregate variable. All variables with a "+" should sum up to the super-variable corresponding to the part of the name before the "+". For example:

```
FE = FE|+|Electricity + FE|+|Heat + FE|+|Hydrogen + FE|+|Solids ...
```

However, multiple stages of summations should only feature one "+" at the lowest level of aggregation and not multiple "+" levels (so ``Emi|CO2|Energy|+|Demand`` instead of `Emi|CO2|+|Energy|+|Demand`). If there are multiple different variable groups that belong to one super variable, additional pluses are added to distinguish them for the other variable groups. For example:

```
FE = FE|+|Electricity + FE|+|Heat + FE|+|Hydrogen + FE|+|Solids ...

FE = FE|++|Buildings + FE|++|Industry + FE|++|Transport ...
```

You can check out the different summation groups of variables with "+" by running the function ``extractVariableGroups()`` from the [mip-package](https://github.com/pik-piam/mip#readme). The argument to the function is a vector of variable names. For example:

```r
mip::extractVariableGroups(getNames(testReport, dim=3))
```

The REMIND reporting will provide you with two different mif-files. The standard mif-file will come with "+" notation, while a second mif-file "without_plus" is generated where pluses were deleted to ease further processing. 

Note that the defintions of REMIND variables as generated by the ``remind2`` library may not correspond to the definition of equally or similarly named variables in standardized model intercomparison projects. For mappings of REMIND variables to variable templates from model intercomparison projects, please see the pik-piam library [project_interfaces](https://github.com/pik-piam/project_interfaces). 

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **remind2** in publications use:

Rodrigues R (2022). _remind2: The REMIND R package (2nd generation)_. R package version 1.86.12, <URL: https://github.com/pik-piam/remind2>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {remind2: The REMIND R package (2nd generation)},
  author = {Renato Rodrigues},
  year = {2022},
  note = {R package version 1.86.12},
  url = {https://github.com/pik-piam/remind2},
}
```
