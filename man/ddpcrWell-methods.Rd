% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numDroplets.R
\name{isEmpty,ddpcrWell-method}
\alias{isEmpty,ddpcrWell-method}
\alias{isEmpty,ddpcrPlate-method}
\title{Is a \code{\link{ddpcrWell}} object empty?}
\usage{
\S4method{isEmpty}{ddpcrWell}(x)

\S4method{isEmpty}{ddpcrPlate}(x)
}
\arguments{
\item{x}{An object to test for emptiness.}
}
\value{
A logical value.
}
\description{
Returns a logical value as to whether the given object has no
droplets/wells.
}
\examples{
## Check that an empty ddpcrPlate object is in fact empty.
krasPlate <- ddpcrPlate(wells=list())
isEmpty(krasPlate)

## Now add some amplitude data and check that is not empty.
krasPlate <- ddpcrPlate(wells=KRASdata)
isEmpty(krasPlate)

}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
