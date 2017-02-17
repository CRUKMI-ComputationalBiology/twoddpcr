#' @include ddpcrPlate.R
#' @include ddpcrWell.R
#' @import methods
#'
NULL

#' Retrieve the cluster centres.
#'
#' @description \code{clusterCentres} retrieves the cluster centres for 
#' a \code{\link{ddpcrWell}} object or the centres for each well in 
#' a \code{\link{ddpcrPlate}} object.
#' 
#' @param theObject A \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}} 
#' object.
#' @param cMethod The classification method for which to obtain the centres.
#'
#' @return If a \code{ddpcrWell} object is given, \code{clusterCentres} returns 
#' the cluster centres as a data frame.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#' 
#' @examples
#' ## Get the centres of a sample with 4 clusters.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' clusterCentres(aWell, "Cluster")
#'
#' ## Get the centres of a sample with 3 clusters.
#' aWell <- ddpcrWell(well=KRASdata[["H04"]])
#' clusterCentres(aWell, "Cluster")
#'
#' ## Retrieve the cluster centres of each of the wells in a \code{ddpcrPlate} 
#' ## object.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' clusterCentres(krasPlate, cMethod="Cluster")
#'
#' ## Retrieve the cluster centres of all wells combined.
#' combinedCentres(krasPlate, cMethod="Cluster")
#'
#' @name clusterCentres
#'
#' @aliases clusterCenters wellCentres wellCenters
#'
#' @export

setGeneric("clusterCentres", function(theObject, cMethod)
  {
    standardGeneric("clusterCentres")
  }
)

#' @rdname clusterCentres
#'
#' @exportMethod clusterCentres

setMethod("clusterCentres", "ddpcrWell", function(theObject, cMethod)
  {
    wellCl <- wellClassification(theObject, withAmplitudes=TRUE)
    classMeans(wellCl, classCol=cMethod)
  }
)

#' @rdname clusterCentres
#'
#' @return If a \code{ddpcrPlate} object is given, \code{clusterCentres} return 
#' a list of data frames, where each data frame corresponds to the cluster 
#' centres of a well.
#'
#' @aliases plateCentres plateCenters
#'
#' @exportMethod clusterCentres

setMethod("clusterCentres", "ddpcrPlate", function(theObject, cMethod)
  {
    lapply(theObject, clusterCentres, cMethod)
  }
)


#' @rdname clusterCentres
#'
#' @description \code{combinedCentres} retrieves the cluster centres for all of 
#' the wells together.
#' 
#' @return \code{combinedCentres} returns a data frame of the centres of all of 
#' the wells combined.
#'
#' @aliases combinedCenters combinedPlateCentres combinedPlateCenters
#'
#' @export

setGeneric("combinedCentres", function(theObject, cMethod)
  {
    standardGeneric("combinedCentres")
  }
)

#' @rdname clusterCentres
#'
#' @exportMethod combinedCentres

setMethod("combinedCentres", "ddpcrPlate", function(theObject, cMethod)
  {
    cl <- do.call(rbind, plateClassification(theObject, withAmplitudes=TRUE))
    classMeans(cl, classCol=cMethod)
  }
)


