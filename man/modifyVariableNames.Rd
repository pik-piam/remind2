% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modifyVariableNames.R
\name{modifyVariableNames}
\alias{modifyVariableNames}
\title{Modify Variable Names}
\usage{
modifyVariableNames(x, modify, writeMifSuffix = NULL, ...)
}
\arguments{
\item{x}{A path to a mif-file (might be created by confGDX2MIF), a magpie
object containing reporting, a quitte object, or a character vector or
factor of variable names.}

\item{modify}{A function that modifies a character vector.}

\item{writeMifSuffix}{\code{NULL} or a suffix for the file name if the result
should be written as a mif-file.}

\item{...}{Further arguments for the modify function.}
}
\value{
If \code{x} is a file path and \code{writeMifSuffix} is \code{NULL},
  a MAgPIE object. If \code{x} is not a file path the result is the object
  \code{x} with replaced variable names.
}
\description{
Modify Variable Names
}
