#' @import methods
NULL

#' @title K-means classify the wells in a \code{ddpcrWell} or 
#' \code{ddpcrPlate} object, or in a data frame.
#' 
#' @param droplets A \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}} 
#' object, or a data frame with columns \code{Ch1.Amplitude} and 
#' \code{Ch2.Amplitude}.
#' @param centres Either:
#' \itemize{
#'   \item A matrix corresponding to the initial centres to use for the k-means 
#'   algorithm; or
#'   \item An integer corresponding to the number of clusters. If this is set, 
#'   the initial centres are randomly set.
#' }
#' Defaults to \code{matrix(c(0, 0, 10000, 0, 0, 7000, 10000, 7000), ncol=2, 
#' byrow=TRUE)}
#' @param ... Other options depending on the type of \code{droplets}.
#'
#' @return An object with the new classification.
#'
#' @seealso This method uses the \code{\link[stats]{kmeans}} function.
#' @seealso To manually set and retrieve classifications, use the 
#' \code{\link{wellClassification}}, \code{\link{plateClassification}} and
#' \code{\link{plateClassificationMethod}} methods.
#' @seealso For a supervised classification approach, one may want to consider 
#' \code{\link{knnClassify}}.
#'
#' @name kmeansClassify
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ### Use the KRASdata dataset for all of these examples.
#'
#' ## Use K-means clustering to classify droplets into four (the default
#' ## number) classes.
#' aWell <- kmeansClassify(KRASdata[["E03"]])
#'
#' ## We can look the the classification or the centres.
#' head(aWell$data)
#' aWell$centres
#'
#' ## Specify 3 centres for a different sample in KRASdata.
#' aWell <- kmeansClassify(KRASdata[["H04"]], centres=3)
#' head(aWell$data)
#'
#' ## We can be more specific with the choice of centres.
#' aWell <- kmeansClassify(KRASdata[["H04"]],
#'                        centres=matrix(c(5000, 1500, 5500, 7000, 10000, 
#'                                         2000), ncol=2, byrow=TRUE))
#'
#' ## We can use \code{ddpcrWell} objects directly as a parameter.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' kmeansClassify(aWell)
#'
#' ## We can take multiple samples in a \code{ddpcrPlate} object and
#' ## classify everything together.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' kmeansClassify(krasPlate)
#'
#' @export kmeansClassify

setGeneric("kmeansClassify",
  function(droplets,
           centres=matrix(c(0, 0, 10000, 0, 0, 7000, 10000, 7000),
                          ncol=2, byrow=TRUE),
           ...)
  {
    standardGeneric("kmeansClassify")
  }
)


#' @rdname kmeansClassify
#'
#' @description If \code{droplets} is a data frame, the droplets are classified 
#' using the k-means clustering algorithm.
#'
#' @param fullTable If \code{TRUE}, returns a full data frame of droplets and 
#' their classification; if \code{FALSE}, simply returns a factor corresponding 
#' to this classification. Defaults to \code{TRUE}.
#'
#' @return If \code{droplets} is a data frame, a list is returned with the 
#' following components:
#'   \item{data}{A data frame or vector corresponding to the classification.}
#'   \item{centres}{A data frame listing the final centre points from the 
#'   k-means algorithm with the corresponding cluster labels.}
#'
#' @importFrom stats kmeans
#' @exportMethod kmeansClassify

setMethod("kmeansClassify", "data.frame",
  function(droplets,
           centres=matrix(c(0, 0, 10000, 0, 0, 7000, 10000, 
                            7000), ncol=2, byrow=TRUE),
           fullTable=TRUE)
  {
    if(is.matrix(centres) || is.data.frame(centres))
    {
      if(nrow(centres) == 0 || nrow(centres) > 4)
        stop("The parameter 'centres' should be a matrix with 1, 2, 3 or 4 ",
             "rows.")
    }
    else if(is.numeric(centres) && centres %% 1 == 0)
    {
      if(centres <= 0 || centres > 4)
        stop("The parameter 'centres' should be 1, 2, 3, 4 or a matrix.")
    }
    else
      stop("The parameter 'centres' should be a matrix or integer.")
      
    # Use the k-means algorithm to classify.
    fit <- kmeans(droplets[, c("Ch1.Amplitude", "Ch2.Amplitude")], centres)
    df <- data.frame("Ch1.Amplitude"=droplets$Ch1.Amplitude,
                     "Ch2.Amplitude"=droplets$Ch2.Amplitude,
                     "class"=fit$cluster)

    # Get the centres of the clusters found by k-means.
    finalCentres <- data.frame(fit$centers,
                               "class"=c(seq_len(nrow(fit$centers))))

    # Relabel everything.
    if(!is.null(rownames(centres)) &
       all(rownames(centres) %in% ddpcr$classes))
    {
      df$class <- relabelClasses(df, presentClasses=rownames(centres))
      finalCentres$class <-
        relabelClasses(finalCentres, presentClasses=rownames(centres))
    }
  else
  {
    df$class <- relabelClasses(df)
    finalCentres$class <- relabelClasses(finalCentres)
  }

  if(fullTable)
    list(data=df, centres=finalCentres)
  else
    list(data=df$class, centres=finalCentres)
  }
)


#' @rdname kmeansClassify
#'
#' @description For \code{ddpcrWell}, the droplets are classified by using the 
#' k-means clustering algorithm.
#'
#' @exportMethod kmeansClassify

setMethod("kmeansClassify", "ddpcrWell",
  function(droplets, centres=matrix(c(0, 0, 10000, 0, 0, 7000, 10000, 7000),
                                    ncol=2, byrow=TRUE))
  {
    # Get new classification and assign it.
    df <- amplitudes(droplets)
    km <- kmeansClassify(df, centres=centres, fullTable=FALSE)
    wellClassification(droplets, "kmeans") <- km$data

    validObject(droplets)
    return(droplets)
  }
)


#' @rdname kmeansClassify
#'
#' @description For \code{ddpcrPlate}, all of the wells are combined and 
#' classified, with this new classification assigned to the 
#' \code{ddpcrPlate} object.
#' 
#' @exportMethod kmeansClassify

setMethod("kmeansClassify", "ddpcrPlate",
  function(droplets, centres=matrix(c(0, 0, 10000, 0, 0, 7000, 10000, 7000),
                                    ncol=2, byrow=TRUE))
  {
    # Combine everything and classify.
    df <- do.call(rbind, amplitudes(droplets))
    km <- kmeansClassify(df, centres=centres, fullTable=FALSE)
    plateClassification(droplets, "kmeans") <- km$data

    validObject(droplets)
    return(droplets)
  }
)

