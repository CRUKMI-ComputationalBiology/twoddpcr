% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseCounts.R
\name{castSummary}
\alias{castSummary}
\title{Makes a long form data frame into wide form.}
\usage{
castSummary(df, ch1Label = "Mt", ch2Label = "Wt", rows = NULL)
}
\arguments{
\item{df}{A data frame created by calling \code{read.csv} on the raw ddPCR
output.}

\item{ch1Label}{The prefix to use for the channel 1 target. Defaults to
"Mt".}

\item{ch2Label}{The prefix to use for the channel 2 target. Defaults to
"Wt".}

\item{rows}{The number of rows to retain from the original data frame. If
\code{NULL}, all of the wells are used. Defaults to \code{NULL}.}
}
\value{
A data frame with the target rows merged.
}
\description{
Returns a data frame with the dependent columns prefixed with given labels
(depending on the targets). All relevant columns are retained.
}
\examples{
## Cast output from Bio-Rad's QuantaSoft into wide form.
castSummary(KRAScountsQS)

## Only retain selected rows.
castSummary(KRAScountsQS, rows=c(1,4:6))

}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
