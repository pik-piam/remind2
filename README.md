# The REMIND R package (2nd generation)

R package **remind2**, version **1.86.12**

[![CRAN status](https://www.r-pkg.org/badges/version/remind2)](https://cran.r-project.org/package=remind2)  [![R build status](https://github.com/pik-piam/remind2/workflows/check/badge.svg)](https://github.com/pik-piam/remind2/actions) [![codecov](https://codecov.io/gh/pik-piam/remind2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/remind2) [![r-universe](https://pik-piam.r-universe.dev/badges/remind2)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Contains functions for creating the reporting of the REMIND model in form of a .mif-file. It includes the function ``convGDX2MIF()`` which creates the main REMIND output contained in the mif-file based on the fulldata.gdx file of a REMIND run. The calculation of the REMIND variables as reported in the mif-file is separated into different functions per variable category (e.g. ``SE|...`` variables are calculated in reportSE.R, ``Emi|...`` variables in reportEmi.R etc.). 

## REMIND Variable Naming

There are a couple of naming conventions for REMIND variables in the mif-file:

* variables typically follow the structure ``Variable Type|...|Sector1|Sector2|…|Energy Carrier Output|Energy Carrier Origin`` (Example: ``FE|Industry|Liquids|Biomass`` contains liquid fuel used as final energy in industry that is produced from biomass)
* variables including a "+" should sum up to the variable written before the "+". Multiple stages of summations should only feature a "+" at the lowest level of aggregation (see details below), the variables including a "+" are subject to an automated summation check by ``test-convGDX2mif.R`` when building the library. 
* modifications of standard variables like ``w/ Bunkers``, ``before taxes`` or "gross" should be placed at the highest relevant aggregation level (examples: ``Emi|CO2|Gross|Energy|Supply|+|Liquids``, ``FE|w/ Bunkers|+|Transport``)
* variables added solely for diagnostic or debugging purpose which should not be used for projects, papers etc. should start with "Internal|..."

* "+" logic in detail + examples

* note that, REMIND variable definition not necessarily the same for similarly named varibles as in IAM comparison projects. For current mappings of REMIND variables to IAM comparison projects see project interfactes

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
