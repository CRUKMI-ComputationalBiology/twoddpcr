% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseCounts.R
\name{.getWellNames}
\alias{.getWellNames}
\title{Extract the well names from a data frame.}
\usage{
.getWellNames(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
A factor of row names.
}
\description{
Get the well names from a data frame, checking if there is a \code{Well}
column or by using the row numbering.
}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
