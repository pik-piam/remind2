% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.reportEntry.R
\name{read.reportEntry}
\alias{read.reportEntry}
\title{Read entry in file of report format}
\usage{
read.reportEntry(outputdir, entry, type = NULL)
}
\arguments{
\item{outputdir}{output folder which contains the reporting}

\item{entry}{entry of the reporting that you want to read in}

\item{type}{type of the reporting that you want to read in}
}
\description{
This function reads one entry of a reporting file (a file in the model
intercomparison file format *.mif) into a MAgPIE object. This function can
be used by readAll() to read in the data for more than one output folder
}
\examples{
\dontrun{
read.reportEntry("output/SSP2-ref", entry = "Emi|Kyoto Gases (Mt CO2eq/yr)")
  remind2:::readAll(outputdirs, read.reportEntry, entry = "Emi|Kyoto Gases (Mt CO2eq/yr)",
                    asList = FALSE)
}
}
\author{
Lavinia Baumstark, David Klein
}
