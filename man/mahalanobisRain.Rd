% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mahRain.R
\name{mahalanobisRain}
\alias{mahalanobisRain}
\alias{multivariateRain}
\alias{multivariateNormalRain}
\alias{mvNormalRain}
\alias{mvnRain}
\alias{mahalanobisRain,data.frame-method}
\alias{mahalanobisRain,ddpcrWell-method}
\alias{mahalanobisRain,ddpcrPlate-method}
\title{Define 'rain' (unclassified) droplets by fitting the clusters to
bivariate normal distributions.}
\usage{
mahalanobisRain(droplets, cMethod, maxDistances = 30, ...)

\S4method{mahalanobisRain}{data.frame}(droplets, cMethod, maxDistances = 30, fullTable = TRUE)

\S4method{mahalanobisRain}{ddpcrWell}(droplets, cMethod, maxDistances = 30)

\S4method{mahalanobisRain}{ddpcrPlate}(droplets, cMethod, maxDistances = 30)
}
\arguments{
\item{droplets}{A \code{ddpcrWell} or \code{ddpcrPlate} object, or
a data frame of droplets with "Ch1.Amplitude" and "Ch2.Amplitude" columns,
as well as a class column.}

\item{cMethod}{The name or column number of the classification for which we
want to add rain to.}

\item{maxDistances}{A list of (levels) with keys in \code{c("NN", "PN",
"NP", "PP")}. If the list is empty or set as \code{NULL}, no rain is added.
If set as a single integer \code{n}, this value is taken for all classes,
i.e. \code{list("NN"=n, "PN"=n, "NP"=n, "PP"=n)}.}

\item{...}{Other options depending on the type of \code{droplets}.}

\item{fullTable}{If \code{TRUE}, returns a full data frame of droplets and
their classification; if \code{FALSE}, simply returns a factor corresponding
to this classification. Defaults to \code{TRUE}.}
}
\value{
An object where the specified class has "Rain" entries added.
}
\description{
Assume that each of the classified clusters are bivariate normally
distributed. We add fuzziness to the classifications by assigning droplets
far away from the centres as "Rain". We use the Mahalanobis distance for
each cluster to determine whether a droplet is 'too far away'.
}
\examples{
## Take a data frame of droplets of transform it into the rigth format.
droplets <- KRASdata[["E03"]]
droplets$Cluster <- relabelClasses(droplets, classCol="Cluster")

## Add rain as a new column.
droplets$ClusterMahRain <-
    mahalanobisRain(droplets, cMethod="Cluster", fullTable=FALSE)
table(droplets$ClusterMahRain)

## The maximum distance around each mean can be changed uniformly.
droplets$ClusterMahRain <-
    mahalanobisRain(droplets, cMethod="Cluster", maxDistances=35,
                    fullTable=FALSE)
table(droplets$ClusterMahRain)

## Or we can change the maximum distances for each individual cluster.
droplets$ClusterMahRain <-
    mahalanobisRain(droplets, cMethod="Cluster",
                    maxDistances=list(NN=35, NP=30, PN=30, PP=30),
                    fullTable=FALSE)
table(droplets$ClusterMahRain)

# This method works the same for ddpcrWell objects.
aWell <- ddpcrWell(well=KRASdata[["E03"]])
aWell <- mahalanobisRain(aWell, cMethod="Cluster")
table(wellClassification(aWell, cMethod="ClusterMahRain"))

# Likewise for ddpcrPlate objects.
krasPlate <- ddpcrPlate(wells=KRASdata[c("E03", "H03", "C04", "F04")])
krasPlate <- mahalanobisRain(krasPlate, cMethod="Cluster")
lapply(plateClassification(krasPlate, cMethod="ClusterMahRain"), table)

}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
