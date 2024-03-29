% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareCalibrationTargets.R
\name{compareCalibrationTargets}
\alias{compareCalibrationTargets}
\title{Render compareCalibrationTargets}
\usage{
compareCalibrationTargets(
  gdxPaths,
  outputDir = getwd(),
  outputFile = "compareCalibrationTargets.html"
)
}
\arguments{
\item{gdxPaths}{\code{character(n)}, optionally named. Paths to GDX objects.
If the vector has names, those are used to refer to the scenarios in the
output file.}

\item{outputDir}{The directory where the output document
and intermediary files are created.}

\item{outputFile}{File name (without extension) of the
output document to be created.}
}
\value{
The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
}
\description{
Render compareCalibrationTargets
}
\examples{
\dontrun{
# Simple use.
checkVsCalibData(
  gdxPaths = c("path/to/fulldata.gdx", "another path/to/fulldata.gdx"),
  outputDir = "path/to/output/directory",
  outputFile = "myComparison.html"
)
}
}
\author{
Falk Benke
}
