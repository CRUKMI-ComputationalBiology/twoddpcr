% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterStats.R
\name{classCov}
\alias{classCov}
\title{Get the covariance of each cluster.}
\usage{
classCov(droplets, classCol = "class")
}
\arguments{
\item{droplets}{A data frame of droplets with \code{Ch1.Amplitude} and
\code{Ch2.Amplitude} columns, as well as a class column (see the parameter
\code{classCol}).}

\item{classCol}{The column (name or number) from \code{droplets}
representing the class.}
}
\value{
A list of covariance matrices of each cluster.
}
\description{
After classifying droplets, we can compute the covariance for each class.
}
\examples{
## Get the covariance matrix of the clusters.
aWell<- KRASdata[["E03"]]
aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
classCov(aWell, classCol="Cluster")

## We repeat the above but with a sample with no "PP" cluster.
aWell <- KRASdata[["H04"]]
aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
classCov(aWell, classCol="Cluster")

}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
