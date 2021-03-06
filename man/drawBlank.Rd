% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dropletPlot.R
\name{drawBlank}
\alias{drawBlank}
\title{Plot nothing.}
\usage{
drawBlank()
}
\value{
A blank \code{ggplot} object.
}
\description{
We may sometimes request that the \code{\link{dropletPlot}} method takes an
empty data frame as its argument, e.g. an empty CSV file is loaded. This
normally presents an error, which is not the desired output. This function
plots nothing to show that there was no data to plot.
}
\seealso{
\code{\link{ggplot.well}} and
\code{\link[=ggplot.well]{ggplot.plate}} are wrappers for plotting
\code{\link{ddpcrWell}} and \code{\link{ddpcrPlate}} objects.

If there is at least one droplet, the \code{\link{dropletPlot}}
method plots droplet amplitude classifications.
}
