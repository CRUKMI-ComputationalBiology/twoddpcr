#' @import stats
NULL

#' Get the mean of each cluster.
#'
#' @description After classifying droplets, we can compute the mean for each 
#' class.
#'
#' @param droplets A data frame of droplets with \code{Ch1.Amplitude} and 
#' \code{Ch2.Amplitude} columns, as well as a classification column (see the 
#' parameter \code{classCol}).
#' @param classCol The column (name or number) from \code{droplets} 
#' representing the class.
#'
#' @return A list or data frame of means of each class.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get the means of the clusters.
#' aWell <- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#' classMeans(aWell, classCol="Cluster")
#'
#' ## We repeat the above but with a sample with no "PP" cluster.
#' aWell <- KRASdata[["H04"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#' classMeans(aWell, classCol="Cluster")
#'
#' @export

classMeans <- function(droplets, classCol="class")
{
  means <- stats::aggregate(droplets[, c("Ch1.Amplitude", "Ch2.Amplitude")],
                            by=list(droplets[, classCol]), mean)
  data.frame(means, row.names=1)
}


#' Get the covariance of a cluster.
#'
#' Get the covariance of a single cluster.
#'
#' @param cl The cluster of which to find the covariance.
#' @param droplets A data frame of droplets with \code{Ch1.Amplitude} and 
#' \code{Ch2.Amplitude} columns, as well as a class column (see the parameter 
#' \code{classCol}).
#' @param classCol The column (name or number) from \code{droplets} 
#' representing the class.
#'
#' @return The covariance matrix of the chosen cluster. If not defined, return 
#' \code{NULL}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.cov <- function(cl, droplets, classCol)
{
  # Find covariance of each cluster.
  covariance <-
    stats::cov(droplets[droplets[, classCol] == cl,
               c("Ch1.Amplitude", "Ch2.Amplitude")])

  # Return NULL if there are no droplets in the class.
  if(anyNA(covariance))
    return(NULL)
  else
    return(covariance)
}


#' Get the covariance of each cluster.
#'
#' After classifying droplets, we can compute the covariance for each class.
#'
#' @param droplets A data frame of droplets with \code{Ch1.Amplitude} and 
#' \code{Ch2.Amplitude} columns, as well as a class column (see the parameter 
#' \code{classCol}).
#' @param classCol The column (name or number) from \code{droplets} 
#' representing the class.
#'
#' @return A list of covariance matrices of each cluster.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get the covariance matrix of the clusters.
#' aWell<- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#' classCov(aWell, classCol="Cluster")
#'
#' ## We repeat the above but with a sample with no "PP" cluster.
#' aWell <- KRASdata[["H04"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#' classCov(aWell, classCol="Cluster")
#'
#' @export

classCov <- function(droplets, classCol="class")
{
  covs <- lapply(ddpcr$classes, .cov, droplets, classCol)
  setNames(covs, ddpcr$classes)
}


#' Get the inverse of a matrix
#'
#' Given a matrix, compute the inverse or return \code{NULL} if it is singular.

.matrixInverse <- function(s)
{
  if(is.null(s))
    return(NULL)
  else
    tryCatch({
      inv <- solve(s, matrix(c(1,0,0,1), 2, 2))
      colnames(inv) <- rownames(s)
      inv
    },
    error=function(e) { NULL })
}

#' Get some basic statistical properties for each class.
#'
#' This function gives the mean, covariance and inverse of the covariance for 
#' each of the classes.
#'
#' @param droplets A data frame of droplets with \code{Ch1.Amplitude} and 
#' \code{Ch2.Amplitude} columns, as well as a class column (see the parameter 
#' \code{classCol}).
#' @param classCol The column (name or number) from \code{droplets} 
#' representing the class.
#'
#' @return A list (grouped by class name) of lists with keys \code{mean}, 
#' \code{cov} and \code{cov.inv}. If \code{cov} is a singular matrix, then 
#' \code{cov.inv} will be \code{NULL}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get some basic statistical properties of the clusters.
#' aWell <- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#' classStats(aWell, classCol="Cluster")
#'
#' ## We repeat the above but with a sample with no "PP" cluster.
#' aWell <- KRASdata[["H04"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#' classStats(aWell, classCol="Cluster")
#'
#'
#'
#'
#' @export

classStats <- function(droplets, classCol="class")
{
  means <- classMeans(droplets, classCol)
  covs <- classCov(droplets, classCol)
  
  # Compute the inverses of the covariance matrices.
  covs.inv <- lapply(covs, .matrixInverse)
  
  # Return everything as a list with class as the key, followed by mean, cov 
  # and cov.inv.
  s <- lapply(ddpcr$classes,
              function(cl)
              {
                m <- t(means[cl, ])
                names(m) <- c("Ch1.Amplitude", "Ch2.Amplitude")
                list("mean"=m,
                     "cov"=covs[[cl]],
                     "cov.inv"=covs.inv[[cl]])
              })
  setNames(s, ddpcr$classes)
}


