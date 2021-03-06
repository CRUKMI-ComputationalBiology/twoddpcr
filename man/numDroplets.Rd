% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numDroplets.R
\name{numDroplets}
\alias{numDroplets}
\alias{numberDroplets}
\alias{numDrops}
\alias{numberDrops}
\alias{numberOfDroplets}
\alias{numOfDroplets}
\alias{numberOfDrops}
\alias{numDroplets,ddpcrWell-method}
\alias{numDroplets,ddpcrPlate-method}
\title{Retrieve the number of droplets.}
\usage{
numDroplets(theObject, ...)

\S4method{numDroplets}{ddpcrWell}(theObject)

\S4method{numDroplets}{ddpcrPlate}(theObject)
}
\arguments{
\item{theObject}{A \code{\link{ddpcrPlate}} object.}

\item{...}{Other parameters depending on the type of \code{theObject}.}
}
\value{
For \code{ddpcrWell} objects, return the number of droplets as an
integer.

For \code{ddpcrPlate} objects, return a named vector. The names
correspond to a well name and each item corresponding to the number of
droplets in that well.
}
\description{
Retrieves the number of droplets in a \code{\link{ddpcrWell}} or
\code{\link{ddpcrPlate}} object.
}
\examples{
## Count the number of droplets in a well.
aWell <- ddpcrWell(well=KRASdata[["E03"]])
numDroplets(aWell)

## Get all of the wells in a named vector.
krasPlate <- ddpcrPlate(wells=KRASdata)
(numberDroplets <- numDroplets(krasPlate))
sum(numberDroplets)

## We can choose to get a subset of the wells.
(numberDroplets <- numDroplets(krasPlate[c("H03", "A04")]))
sum(numberDroplets)

}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
