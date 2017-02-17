#' @import class
#' @import methods
NULL

#' @title Use the k-nearest neighbour algorithm to classify the wells in 
#' a \code{ddpcrWell} or \code{ddpcrPlate} object, or in a data frame.
#'
#' @param droplets A \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}} 
#' object, or a data frame with columns \code{Ch1.Amplitude} and 
#' \code{Ch2.Amplitude}.
#' @param trainData A data frame of training data with columns 
#' \code{Ch1.Amplitude} and \code{Ch2.Amplitude}.
#' @param cl A vector of classes corresponding to \code{trainData}.
#' @param k The number of nearest neighbours to use in the algorithm.
#' @param prob The minimal proportion of votes for the winning class needed to 
#' assert that a droplet belongs to the class. This figure should be a float 
#' between 0 and 1. For example, if 0.6 then at least 60% of a droplet's 
#' k-nearest neighbours need to be of one class, otherwise it is classified as 
#' "Rain". Defaults to 0, i.e. we do not use "Rain".
#' @param ... Other options depending on the type of \code{droplets}.
#'
#' @return An object with the new classification.
#'
#' @seealso This method uses the \code{\link[class]{knn}} function.
#' @seealso To manually set and retrieve classifications, use the 
#' \code{\link{wellClassification}}, \code{\link{plateClassification}} and
#' \code{\link{plateClassificationMethod}} methods.
#' @seealso \code{kmeansClassify} provides a wrapper for the k-means clustering 
#' algorithm.
#'
#' @name knnClassify
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ### Use the KRASdata dataset for all of these examples.
#'
#' ## Use k-means clustering to classify one sample. Use this as training
#' ## data for the K-Nearest Neighbour algorithm.
#' trainingData <- KRASdata[["E03"]]
#' trainingData <- kmeansClassify(trainingData)$data
#'
#' ## Classify a dataframe using k-NN with k = 1 and the above training data.
#' aWell <- knnClassify(
#'            KRASdata[["F03"]],
#'            trainData=trainingData[, c("Ch1.Amplitude", "Ch2.Amplitude")],
#'            cl=trainingData$class,
#'            k=1)
#' dropletPlot(aWell, cMethod="class")  # visualising the classification
#'
#' ## We can change k to a larger number, here with a ddpcrWell object.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' aWell <- knnClassify(
#'            aWell,
#'            trainData=trainingData[, c("Ch1.Amplitude", "Ch2.Amplitude")],
#'            cl=trainingData$class,
#'            k=3)
#' dropletPlot(aWell, cMethod="knn")  # visualising the classification
#'
#' ## Changing the 'prob' parameter means that droplets with less than 'prob'
#' ## of the votes will not be classified. We do this for a ddpcrPlate
#' ## object.
#' krasPlate <- ddpcrPlate(wells=KRASdata[c("E03", "H03", "C04", "F04")])
#' krasPlate <- knnClassify(
#'                krasPlate,
#'                trainData=trainingData[, c("Ch1.Amplitude", "Ch2.Amplitude")],
#'                cl=trainingData$class,
#'                k=3,
#'            prob=0.6)
#' dropletPlot(krasPlate, cMethod="knn")  # visualising the classification
#'
#' @export

setGeneric("knnClassify", function(droplets, trainData, cl, k, prob=0.0, ...)
  {
    standardGeneric("knnClassify")
  }
)


#' @rdname knnClassify
#'
#' @description If \code{droplets} is a data frame, the droplets are classified 
#' using the k-nearest neighbour algorithm against a training data set.
#'
#' @param fullTable If \code{TRUE}, returns a full data frame of droplets and 
#' their classification; if \code{FALSE}, simply returns the classified vector. 
#' Defaults to \code{TRUE}.
#'
#' @return If \code{droplets} is a data frame, return data frame or factor 
#' (depending on the value of \code{fullTable}) of the droplet classification 
#' under the k-NN algorithm.
#'
#' @exportMethod knnClassify

setMethod("knnClassify", "data.frame",
  function(droplets, trainData, cl, k, prob=0.0, fullTable=TRUE)
  {
    # Only take the amplitudes columns into account.
    droplets <- droplets[, c("Ch1.Amplitude", "Ch2.Amplitude")]

    # Use the k-NN algorithm to classify the droplets.
    knnOut <- class::knn(trainData, droplets, cl, k, prob=prob)

    # Add rain if specified.
    if(prob)
    {
      knnOut <- factor(ifelse(attr(knnOut, "prob") >= prob,
                              as.character(knnOut),
                              ddpcr$rain),
                       levels=ddpcr$classesRain)
    }

    if(fullTable)
      data.frame(droplets, "class"=knnOut)
    else
      knnOut
  }
)


#' @rdname knnClassify
#' 
#' @description If \code{droplets} is a \code{ddpcrWell} object, the droplets 
#' in the well are classified and returned in another \code{ddpcrWell} object.
#' 
#' @exportMethod knnClassify

setMethod("knnClassify", "ddpcrWell",
  function(droplets, trainData, cl, k, prob=0.0)
  {
    # Retrieve the droplets and set the classification.
    df <- amplitudes(droplets)
    kn <- knnClassify(droplets=df, trainData=trainData, cl=cl,
                      k=k, prob=prob, fullTable=FALSE)
    wellClassification(droplets, "knn") <- kn

    validObject(droplets)
    return(droplets)
  }
)

#' @rdname knnClassify
#'
#' @description If \code{droplets} is a \code{ddpcrPlate} object, the 
#' wells are combined and classified together, with the resulting 
#' classification assigned to the \code{ddpcrPlate} object.
#'
#' @exportMethod knnClassify

setMethod("knnClassify", "ddpcrPlate",
  function(droplets, trainData, cl, k, prob=0.0)
  {
    # Combine everything and classify.
    df <- do.call(rbind, amplitudes(droplets))
    kn <- knnClassify(droplets=df, trainData=trainData, cl=cl,
                      k=k, prob=prob, fullTable=FALSE)
    plateClassification(droplets, "knn") <- kn

    validObject(droplets)
    return(droplets)
  }
)

