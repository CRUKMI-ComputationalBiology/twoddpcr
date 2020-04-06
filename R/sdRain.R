#' @import methods
NULL

#' Find the standard deviation of droplets (in a given class) multipied by
#' a given constant.
#'
#' For a specified class, take a data frame of droplet amplitudes and compute
#' the standard deviation multiplied by a level of accuracy.
#'
#' @param droplets A data frame of droplets with "Ch1.Amplitude" and
#' "Ch2.Amplitude" columns, as well as a class column (see classCol).
#' @param cl The class to focus on. Typically one of "NN", "PN", "NP" and "PP".
#' @param level A constant by which we will multiply the standard deviation.
#' Defaults to 5.
#' @param classCol The column (name or number) from 'droplets' representing the
#' class.
#'
#' @return A list with named elements 'ch1' and 'ch2', each giving the error
#' bound for the corresponding channel. If the number of droplets is either
#' 0 or 1, return list(ch1=0, ch2=0) to avoid any errors. Otherwise, return
#' level * (sd of the droplets in each channel).
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

getCutOff <- function(droplets, cl, level=3, classCol="class") {
  # We want a class with just one droplet to be in a non-rainy class.
  if(length(droplets) <= 1) {
    list(ch1=0, ch2=0)
  } else {
    ch1Error <-
      level * stats::sd(droplets[droplets[, classCol]==cl, "Ch1.Amplitude"])
    ch2Error <-
      level * stats::sd(droplets[droplets[, classCol]==cl, "Ch2.Amplitude"])
    list("Ch1.Amplitude"=ch1Error, "Ch2.Amplitude"=ch2Error)
  }
}


#' @title Add rain to a classification by using a chosen multiple of standard
#' deviation.
#'
#' @description Although we can use various algorithms to classify all droplets
#' in a ddPCR experiment, there will be some variation between the
#' classifications. We can perhaps have a relatively high confidence that
#' droplets near the centres of clusters do indeed belong to that cluster,
#' whereas we probably have a lower confidence in the classification of those
#' further away, say, near the 'boundary' of two clusters. We may view these
#' droplets (or a subset of them) as having an ambiguous class. This function
#' allows us to only consider droplets classified within a certain distance of
#' the means of each cluster and label the rest as "Rain".
#'
#' @param droplets A \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}}
#' object, or a droplet data frame including a classification column.
#' @param cMethod The name or column number of the classification for which we
#' want to add rain to.
#' @param errorLevel How many multiples of standard deviation from the mean of
#' each cluster to retain. Can be a list where each item corresponds to a class
#' name and the multiple for that class. Can also be a numeric vector of length
#' 1, which is equivalent to a list with all the same entries. Defaults to 5.
#' @param ... Other options depending on the type of \code{droplets}.
#'
#' @name sdRain
#'
#' @references This approach was described in {Jones, M., Williams, J.,
#' Gaertner, K., Phillips, R., Hurst, J., & Frater, J. (2014). Low copy target
#' detection by Droplet Digital PCR through application of a novel open access
#' bioinformatic pipeline, "definetherain." Journal of Virological Methods,
#' 202(100), 46--53. \url{http://doi.org/10.1016/j.jviromet.2014.02.020}}
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Compare the types of droplets in a single well for the "Cluster" class
#' ## and then with rain.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' aWell <- sdRain(aWell, cMethod="Cluster")
#' cl <- wellClassification(aWell)
#' table(cl$Cluster)
#' table(cl$ClusterSdRain)
#'
#' ## Compare the types of droplets in multiple wells for the "Cluster" class
#' ## and then with rain.
#' krasPlate <- ddpcrPlate(wells=KRASdata[c("E03", "H03", "C04", "F04")])
#' krasPlate <- sdRain(krasPlate, cMethod="Cluster")
#' plateSummary(krasPlate, cMethod="Cluster")[, c(1:5)]
#' plateSummary(krasPlate, cMethod="ClusterSdRain")[, c(1:5)]
#'
#' ## The 'errorLevel' parameter can changed.
#' krasPlate <- sdRain(krasPlate, cMethod="Cluster", errorLevel=4)
#' plateSummary(krasPlate, cMethod="ClusterSdRain")[, c(1:5)]
#'
#' ## The 'errorLevel' parameter can also be changed for each cluster.
#' krasPlate <- sdRain(krasPlate, cMethod="Cluster",
#'                     errorLevel=list(NN=5, NP=5, PN=4, PP=3))
#' plateSummary(krasPlate, cMethod="ClusterSdRain")[, c(1:5)]
#'
#' @export

setGeneric("sdRain", function(droplets, cMethod, errorLevel=5, ...) {
  standardGeneric("sdRain")
})


#' @rdname sdRain
#'
#' @param fullTable If \code{TRUE}, returns a full data frame of droplets with
#' an extra column of rainy data; if \code{FALSE}, simply returns a factor
#' where each entry corresponds to an entry in the original classification
#' column with added rain. Defaults to \code{FALSE}.
#'
#' @return If \code{droplets} is a data frame, return a data frame or factor
#' (depending on \code{fullTable}) where droplets with ambiguous
#' classifications are labelled as "Rain".
#'
#' @exportMethod sdRain

setMethod(
  "sdRain", "data.frame",
  function(droplets, cMethod, errorLevel=5, fullTable=TRUE) {
    # Only one number supplied for the error level: make it into a list.
    if(is.numeric(errorLevel) && length(errorLevel) == 1) {
      errorLevel <- rep(errorLevel, length(ddpcr$classes))
      names(errorLevel) <- ddpcr$classes
    }

    means <- classMeans(droplets, classCol=cMethod)
    dfWithRain <- droplets[, cMethod]

    # Only add rain to the main classes that exist in the plot; ignore "Rain"
    # and "N/A".
    classes <- intersect(unique(dfWithRain), ddpcr$classes)
    for(cl in classes) {
      m1 <- means[cl, "Ch1.Amplitude"]
      m2 <- means[cl, "Ch2.Amplitude"]
      sds <- getCutOff(droplets, cl, level=errorLevel[[cl]],
                           classCol=cMethod)

      # We use different inequalities depending on whether the class in each
      # channel is "N" or "P".
      ch1 <- substr(cl, 1, 1)
      ch2 <- substr(cl, 2, 2)

      # Set rain droplets.
      dfWithRain <- ifelse(
        droplets[, cMethod] == cl & (
          (ch1 == "N" & droplets$Ch1.Amplitude > (m1 + sds$Ch1.Amplitude)) |
          (ch1 == "P" & droplets$Ch1.Amplitude < (m1 - sds$Ch1.Amplitude)) |
          (ch2 == "N" & droplets$Ch2.Amplitude > (m2 + sds$Ch2.Amplitude)) |
          (ch2 == "P" & droplets$Ch2.Amplitude < (m2 - sds$Ch2.Amplitude))
        ),
        ddpcr$rain,
        as.character(dfWithRain)
      )
    }

    dfWithRain <- factor(dfWithRain, levels=ddpcr$classesRain)
    if(fullTable) {
      data.frame(droplets, rainy=dfWithRain)
    } else {
      dfWithRain
    }
  }
)


#' @rdname sdRain
#'
#' @return If \code{droplets} is a \code{ddpcrWell} object, return
#' a \code{ddpcrWell} object with a rainy classification.
#'
#' @exportMethod sdRain

setMethod("sdRain", "ddpcrWell", function(droplets, cMethod, errorLevel=5) {
  cl <- wellClassification(droplets, cMethod=cMethod, withAmplitudes=TRUE)
  rainy <- sdRain(cl, cMethod=cMethod, errorLevel=errorLevel, fullTable=FALSE)
  wellClassification(droplets, paste0(cMethod, "SdRain")) <- rainy
  droplets
})


#' @rdname sdRain
#'
#' @return If \code{droplets} is a \code{ddpcrPlate} object, return
#' a \code{ddpcrPlate} object with a rainy classifications.
#'
#' @exportMethod sdRain

setMethod("sdRain", "ddpcrPlate", function(droplets, cMethod, errorLevel=5) {
  cl <- plateClassification(droplets, cMethod=cMethod, withAmplitudes=TRUE)
  cl <- do.call(rbind, cl)
  rainy <- sdRain(cl,
                  cMethod=cMethod, errorLevel=errorLevel, fullTable=FALSE)
  plateClassification(droplets, paste0(cMethod, "SdRain")) <- rainy
  droplets
})
