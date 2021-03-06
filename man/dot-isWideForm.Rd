% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseCounts.R
\name{.isWideForm}
\alias{.isWideForm}
\title{Checks a data frame is a wide-form table.}
\usage{
.isWideForm(df, ch1Label = "Mt", ch2Label = "Wt")
}
\arguments{
\item{df}{A data frame.}

\item{ch1Label}{The prefix to use for the channel 1 target. Defaults to
"Mt".}

\item{ch2Label}{The prefix to use for the channel 2 target. Defaults to
"Wt".}
}
\value{
\code{TRUE} if \code{df} is considered to be of the correct format
and \code{FALSE} otherwise.
}
\description{
Our preferred data frame format is to have things in a wide-form data frame,
i.e. to have channel 1 and channel 2 data both in the same row.
}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
