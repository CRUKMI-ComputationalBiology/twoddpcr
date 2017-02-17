#' @import methods
NULL

#' Retrieve a data frame of droplet amplitudes with droplets of a given class 
#' removed.
#'
#' By default, all droplets classified as "N/A" or "Rain" will be removed. 
#' Including these droplets is useful for visualisation purposes, but they 
#' could be a problem in some scenarios, e.g. if we wish to use the 
#' classification as a training data set.
#'
#' @param droplets A \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}} 
#' object, or a data frame with columns \code{Ch1.Amplitude}, 
#' \code{Ch2.Amplitude} and a classification column.
#' @param ... Other parameters depending on the type of \code{droplets}.
#' @param classesToRemove A vector of character strings corresponding to the 
#' classes that should be removed. Defaults to \code{NULL}, i.e. remove 
#' nothing.
#' @param keepUnclassified A logical flag determining whether unclassified 
#' droplets (i.e. "Rain" or "N/A") should be kept. Defaults to \code{FALSE}, 
#' i.e. remove them.
#' 
#' @return If a \code{ddpcrWell} object is given, return a data frame 
#' corresponding to \code{droplets} with the given droplet classes removed.
#' If a \code{ddpcrPlate} object is given, return a list of data frames 
#' instead.
#'
#' @name removeDropletClasses
#'
#' @seealso This function can remove "N/A" droplets from classifications 
#' produced by \code{\link{gridClassify}}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Take a data frame and transform it into the right format.
#' aWell <- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#'
#' ## Add rain using the Mahalanobis distance.
#' aWell$ClusterMahRain <-
#'     mahalanobisRain(aWell, cMethod="Cluster", fullTable=FALSE)
#' table(aWell$ClusterMahRain)
#'
#' ## Suppose we want to use this for training. Remove the "Rain" droplets.
#' aWellCleaned <- removeDropletClasses(aWell, cMethod="ClusterMahRain")
#' table(aWellCleaned$ClusterMahRain)
#'
#' ## All of the above works with ddpcrWell objects.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' aWell <- mahalanobisRain(aWell, cMethod="Cluster")
#' trainingData <- removeDropletClasses(aWell, cMethod="ClusterMahRain")
#' table(wellClassification(aWell, "ClusterMahRain"))
#' table(trainingData$ClusterMahRain)
#'
#' ## Likewise for ddpcrPlate objects we can create the training data.
#' krasPlate <- ddpcrPlate(wells=KRASdata[c("E03", "F03", "G03")])
#' krasPlate <- mahalanobisRain(krasPlate, cMethod="Cluster")
#' trainingData <- removeDropletClasses(krasPlate, cMethod="ClusterMahRain")
#' cl <- plateClassification(krasPlate, cMethod="ClusterMahRain")
#' cl <- unlist(cl)
#' table(cl)
#' td <- do.call(rbind, trainingData)
#' table(td$ClusterMahRain)
#'
#' ## We could also remove other droplet classes, such as the "PN" and "PP"
#' ## clusters.
#' noPNPP <- removeDropletClasses(krasPlate, cMethod="ClusterMahRain",
#'                                classesToRemove=c("PN", "PP"))
#' td <- do.call(rbind, noPNPP)
#' table(td$ClusterMahRain)
#'
#' ## The same could be done, but with the "Rain" retained.
#' noPNPPWithRain <- removeDropletClasses(krasPlate, cMethod="ClusterMahRain",
#'                                        classesToRemove=c("PN", "PP"),
#'                                        keepUnclassified=TRUE)
#' td <- do.call(rbind, noPNPPWithRain)
#' table(td$ClusterMahRain)
#'
#'
#' @export

setGeneric("removeDropletClasses",
  function(droplets, ..., classesToRemove=NULL, keepUnclassified=FALSE)
  {
    standardGeneric("removeDropletClasses")
  }
)


#' @rdname removeDropletClasses
#'
#' @param cMethod This is the name or column number corresponding to the 
#' classification in \code{droplets}. This column should only have entries in 
#' "NN", "PN", "NP, "PP", "Rain" and "N/A". Defaults to "class".
#'
#' @exportMethod removeDropletClasses

setMethod("removeDropletClasses", "data.frame",
  function(droplets, cMethod="class", classesToRemove=NULL,
           keepUnclassified=FALSE)
  {
    df <- droplets[, c("Ch1.Amplitude", "Ch2.Amplitude", cMethod)]
    df <- df[!df[, cMethod] %in% classesToRemove, ]
    if(!keepUnclassified)
      df <- df[!df[, cMethod] %in% c(ddpcr$na, ddpcr$rain), ]
    df
  }
)


#' @rdname removeDropletClasses
#'
#' @exportMethod removeDropletClasses

setMethod("removeDropletClasses", "ddpcrWell",
  function(droplets, cMethod, classesToRemove=NULL, keepUnclassified=FALSE)
  {
    df <- wellClassification(droplets, withAmplitudes=TRUE)
    removeDropletClasses(df, cMethod=cMethod,
                         classesToRemove=classesToRemove,
                         keepUnclassified=keepUnclassified)
  }
)


#' @rdname removeDropletClasses
#'
#' @exportMethod removeDropletClasses

setMethod("removeDropletClasses", "ddpcrPlate",
  function(droplets, cMethod, classesToRemove=NULL, keepUnclassified=FALSE)
  {
    lapply(plateClassification(droplets, withAmplitudes=TRUE),
           removeDropletClasses, cMethod=cMethod,
           classesToRemove=classesToRemove,
           keepUnclassified=keepUnclassified)

  }
)

