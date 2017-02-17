#' @import methods
NULL

#' @title Use a 'grid' to create training data for classification algorithms.
#'
#' @description Classify droplets as "NN", "NP", "PN" or "PP". The 
#' classification is based on upper bounds for negative readings and lower 
#' bounds for positive readings; see the details and parameters for more 
#' detail. If required (see the \code{trainingData} parameter), droplets that 
#' are not classified will be given the label "N/A".
#'
#' @param droplets A \code{\link{ddpcrWell}} object or a data frame of droplet 
#' amplitudes with columns \code{Ch1.Amplitude} and \code{Ch2.Amplitude}.
#' @param ch1NNThreshold The channel 1 upper bound for the NN class. Defaults 
#' to 6500.
#' @param ch2NNThreshold The channel 2 upper bound for the NN class. Defaults 
#' to 1900.
#' @param ch1NPThreshold The channel 1 upper bound for the NP class. Defaults 
#' to 6500.
#' @param ch2NPThreshold The channel 2 lower bound for the NP class. Defaults 
#' to 5000.
#' @param ch1PNThreshold The channel 1 lower bound for the PN class. Defaults 
#' to 10000.
#' @param ch2PNThreshold The channel 2 upper bound for the PN class. Defaults 
#' to 2900.
#' @param ch1PPThreshold The channel 1 lower bound for the PP class. Defaults 
#' to 7500.
#' @param ch2PPThreshold The channel 2 lower bound for the PP class. Defaults 
#' to 5000.
#' @param ... Other options depending on the type of \code{droplets}.
#'
#' @details The \code{threshold} parameters correspond to those in the 
#' following diagram:
#' \preformatted{Ch1 ^        |  |
#'     |        |  |
#'     |   PN   |  |
#'     |        |  |   PP
#'    e|________|  |
#'    g|........:..|_________
#'     |        :  :
#'    c|.............._______
#'    a|______  :  : |
#'     |      | :  : |  NP
#'     |  NN  | :  : |
#'     |      | :  : |
#'     --------------------->
#'            b f  h d    Ch2}
#' Specifically:
#' \describe{
#'   \item{a:}{\code{ch1NNThreshold},}
#'   \item{b:}{\code{ch2NNThreshold},}
#'   \item{c:}{\code{ch1PNThreshold},}
#'   \item{d:}{\code{ch2PNThreshold},}
#'   \item{e:}{\code{ch1NPThreshold},}
#'   \item{f:}{\code{ch2NPThreshold},}
#'   \item{g:}{\code{ch1PPThreshold},}
#'   \item{h:}{\code{ch2PPThreshold}.}
#' }
#'
#' @name gridClassify
#'
#' @seealso \code{\link{thresholdClassify}} is a special case of this
#' function.
#' @seealso \code{\link{removeDropletClasses}} retrieves a data frame with the
#' "N/A" (and "Rain") droplets removed. This can used for transforming 
#' a grid-like classification into usable training data.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Use a grid to set training data for a data frame.
#' sgCl <- gridClassify(KRASdata[["E03"]],
#'                      ch1NNThreshold=5700, ch2NNThreshold=1700,
#'                      ch1NPThreshold=5400, ch2NPThreshold=5700,
#'                      ch1PNThreshold=9700, ch2PNThreshold=2050,
#'                      ch1PPThreshold=7200, ch2PPThreshold=4800)
#' str(sgCl)
#'
#' ## For data frame only, we can set the trainingData flag to FALSE so that
#' ## the unclassified droplets are retained but labelled as "N/A"
#' sgCl <- gridClassify(KRASdata[["E03"]],
#'                      ch1NNThreshold=5700, ch2NNThreshold=1700,
#'                      ch1NPThreshold=5400, ch2NPThreshold=5700,
#'                      ch1PNThreshold=9700, ch2PNThreshold=2050,
#'                      ch1PPThreshold=7200, ch2PPThreshold=4800,
#'                      trainingData=FALSE)
#' dropletPlot(sgCl, cMethod="class")
#'
#' ## The same works for ddpcrWell objects.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' aWell <- gridClassify(aWell,
#'                       ch1NNThreshold=5700, ch2NNThreshold=1700,
#'                       ch1NPThreshold=5400, ch2NPThreshold=5700,
#'                       ch1PNThreshold=9700, ch2PNThreshold=2050,
#'                       ch1PPThreshold=7200, ch2PPThreshold=4800)
#' str(aWell)
#'
#' ## ddpcrPlate objects work in exactly the same way.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' krasPlate <- gridClassify(krasPlate)
#' lapply(plateClassification(krasPlate, withAmplitudes=TRUE), head, n=1)
#'
#' ## The default classification method (column name) is 'gridClassify',
#' ## which may be a bit long. It can be changed.
#' krasPlate <- gridClassify(krasPlate, classMethodLabel="training")
#' lapply(plateClassification(krasPlate, withAmplitudes=TRUE), head, n=1)
#'
#' @export

setGeneric("gridClassify",
  function(droplets,
           ch1NNThreshold=6500, ch2NNThreshold=1900,
           ch1NPThreshold=6500, ch2NPThreshold=5000,
           ch1PNThreshold=10000, ch2PNThreshold=2900,
           ch1PPThreshold=7500, ch2PPThreshold=5000,
           ...)
  {
    standardGeneric("gridClassify")
  }
)

#' @rdname gridClassify
#'
#' @param trainingData Whether to use the output as training data. If 
#' \code{TRUE}, returns the _full table_ with the "N/A" entries removed; if 
#' \code{FALSE}, the "N/A" entries are retained. Taken to be \code{FALSE} if 
#' \code{fullTable} is set to \code{FALSE}. Defaults to \code{TRUE}. Ignored 
#' if \code{droplets} is not a data frame.
#' @param fullTable Whether to return a data frame including amplitude 
#' figures. If \code{TRUE}, a data frame with columns \code{Ch1.Amplitude}, 
#' \code{Ch1.Amplitude} and \code{class} is returned. If \code{FALSE}, a factor 
#' with levels in \code{ddpcr$classesRain} is returned, where each entry 
#' corresponds to each row in \code{droplets} (and \code{trainingData} is 
#' automatically set to \code{FALSE}). Defaults to \code{TRUE}. Ignored if 
#' \code{droplets} is not a data frame.
#' @param naLabel The label to use for unclassified droplets. Should be either 
#' ddpcr$na ("N/A") or ddpcr$rain ("Rain"). Defaults to ddpcr$rain.
#'
#' @return If \code{droplets} is a data frame, return a data frame or factor 
#' (depending on the \code{trainingData} and \code{fullTable} parameters) with 
#' a classification for droplets in the chosen regions.
#'
#' @exportMethod gridClassify

setMethod("gridClassify", "data.frame",
  function(droplets,
           ch1NNThreshold=6500, ch2NNThreshold=1900,
           ch1NPThreshold=6500, ch2NPThreshold=5000,
           ch1PNThreshold=10000, ch2PNThreshold=2900,
           ch1PPThreshold=7500, ch2PPThreshold=5000,
           trainingData=TRUE, fullTable=TRUE,
           naLabel=ddpcr$rain)
  {
    # Check that the thresholds are sensible.
    thresholds <- c(ch1NNThreshold, ch2NNThreshold,
                    ch1NPThreshold, ch2NPThreshold,
                    ch1PNThreshold, ch2PNThreshold,
                    ch1PPThreshold, ch2PPThreshold)
    if(!is.numeric(thresholds) || any(thresholds < 0))
      stop("Thresholds should be nonnegative.")
    if(ch1NNThreshold > ch1PNThreshold || ch2NNThreshold > ch2NPThreshold || 
       (ch1NNThreshold > ch1PPThreshold && ch2NNThreshold > ch2PPThreshold) ||
       (ch1NPThreshold > ch1PNThreshold && ch2NPThreshold < ch2PNThreshold) ||
       ch1NPThreshold > ch1PPThreshold ||
       ch2PNThreshold > ch2PPThreshold)
      stop("The thresholds overlap.")

    # Set the classification of droplets that lie above or below the given 
    # thresholds.
    df <- droplets[, c("Ch1.Amplitude", "Ch2.Amplitude")]
    df$class <-
      ifelse(df$Ch1.Amplitude < ch1NNThreshold
             & df$Ch2.Amplitude < ch2NNThreshold,
        ddpcr$nn,
      ifelse(df$Ch1.Amplitude > ch1PNThreshold
                  & df$Ch2.Amplitude < ch2PNThreshold,
        ddpcr$pn,
      ifelse(df$Ch1.Amplitude < ch1NPThreshold
             & df$Ch2.Amplitude > ch2NPThreshold,
        ddpcr$np,
      ifelse(df$Ch1.Amplitude > ch1PPThreshold
             & df$Ch2.Amplitude > ch2PPThreshold,
        ddpcr$pp,
        naLabel))))

    # If we are setting training data, remove all rows not in the grid.
    if(!fullTable)
      return(factor(df$class, levels=ddpcr$classesRain))
    else if(trainingData)
      df <- df[df$class != ddpcr$na, ]
    
    df$class <- factor(df$class, levels=ddpcr$classesRain)
    df
  }
)


#' @rdname gridClassify
#'
#' @param classMethodLabel A name (as a character string) of the classification 
#' method. Defaults to "grid".
#'
#' @return If \code{droplets} is a \code{ddpcrWell} object, return 
#' a \code{ddpcrWell} object with the appropriate classification.
#'
#' @exportMethod gridClassify

setMethod("gridClassify", "ddpcrWell",
  function(droplets,
           ch1NNThreshold=6500, ch2NNThreshold=1900,
           ch1NPThreshold=6500, ch2NPThreshold=5000,
           ch1PNThreshold=10000, ch2PNThreshold=2900,
           ch1PPThreshold=7500, ch2PPThreshold=5000,
           classMethodLabel="grid",
           naLabel=ddpcr$rain)
  {
    df <- gridClassify(amplitudes(droplets),
                       ch1NNThreshold=ch1NNThreshold,
                       ch2NNThreshold=ch2NNThreshold,
                       ch1NPThreshold=ch1NPThreshold,
                       ch2NPThreshold=ch2NPThreshold,
                       ch1PNThreshold=ch1PNThreshold,
                       ch2PNThreshold=ch2PNThreshold,
                       ch1PPThreshold=ch1PPThreshold,
                       ch2PPThreshold=ch2PPThreshold,
                       trainingData=FALSE,
                       fullTable=TRUE,
                       naLabel=naLabel)

    wellClassification(droplets, classMethodLabel) <- df$class
    droplets
  }
)


#' @rdname gridClassify
#'
#' @return If \code{droplets} is a \code{ddpcrPlate} object, return 
#' a \code{ddpcrPlate} object with the appropriate classification.
#'
#' @exportMethod gridClassify

setMethod("gridClassify", "ddpcrPlate",
  function(droplets,
           ch1NNThreshold=6500, ch2NNThreshold=1900,
           ch1NPThreshold=6500, ch2NPThreshold=5000,
           ch1PNThreshold=10000, ch2PNThreshold=2900,
           ch1PPThreshold=7500, ch2PPThreshold=5000,
           classMethodLabel="grid",
           naLabel=ddpcr$rain)
  {
    df <- gridClassify(do.call(rbind, amplitudes(droplets)),
                       ch1NNThreshold=ch1NNThreshold,
                       ch2NNThreshold=ch2NNThreshold,
                       ch1NPThreshold=ch1NPThreshold,
                       ch2NPThreshold=ch2NPThreshold,
                       ch1PNThreshold=ch1PNThreshold,
                       ch2PNThreshold=ch2PNThreshold,
                       ch1PPThreshold=ch1PPThreshold,
                       ch2PPThreshold=ch2PPThreshold,
                       trainingData=FALSE,
                       fullTable=TRUE,
                       naLabel=naLabel)

    plateClassification(droplets, cMethod=classMethodLabel) <- df$class
    droplets
  }
)


#' @title Set thresholds to classify droplets.
#'
#' @description Classify droplets as "NN", "NP", "PN" or "PP" depending on 
#' whether they lie above or below given thresholds. This is illustrated in the 
#' details and parameters for more detail.
#'
#' @param droplets A \code{\link{ddpcrWell}} object or a data frame of droplet 
#' amplitudes with columns \code{Ch1.Amplitude} and \code{Ch2.Amplitude}.
#' @param ch1Threshold The channel 1 upper bound for the NN and NP classes. 
#' Defaults to 8000
#' @param ch2Threshold The channel 2 upper bound for the NN and PN classes. 
#' Defaults to 3000.
#' @param ... Other options depending on the type of \code{droplets}.
#'
#' @details The \code{threshold} parameters correspond to those in the 
#' following diagram:
#' \preformatted{Ch1 ^          |
#'     |          |
#'     |          |
#'     |    PN    |    PP
#'     |          |
#'     |          |
#'    a|__________|__________
#'     |          |
#'     |          |
#'     |    NN    |    NP
#'     |          |
#'     |          |
#'     --------------------->
#'                b      Ch2}
#' Specifically:
#' \describe{
#'   \item{a:}{\code{ch1Threshold},}
#'   \item{b:}{\code{ch2Threshold}.}
#' }
#'
#' @seealso This function is a special case of \code{\link{gridClassify}}.
#'
#' @name thresholdClassify
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Use thresholds to set a classification for a data frame.
#' gCl <- thresholdClassify(KRASdata[["E03"]],
#'                          ch1Threshold=6789, ch2Threshold=3000)
#' str(gCl)
#'
#' ## The same works for ddpcrWell objects.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' aWell <- thresholdClassify(aWell, ch1Threshold=6789, ch2Threshold=3000)
#' str(aWell)
#'
#' ## ddpcrPlate objects work in exactly the same way.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' krasPlate <- thresholdClassify(krasPlate,
#'                                ch1Threshold=6789, ch2Threshold=3000)
#' lapply(plateClassification(krasPlate, withAmplitudes=TRUE), head, n=1)
#'
#' ## The default classification method (column name) is 'threshold'. It can be
#' ## changed by providing a label.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' krasPlate <- thresholdClassify(krasPlate,
#'                                ch1Threshold=6789, ch2Threshold=3000, 
#'                                classMethodLabel="manual")
#' lapply(plateClassification(krasPlate, withAmplitudes=TRUE), head, n=1)
#'
#' @export

setGeneric("thresholdClassify",
  function(droplets, ch1Threshold=6500, ch2Threshold=2900, ...)
  {
    standardGeneric("thresholdClassify")
  }
)


#' @rdname thresholdClassify
#'
#' @param fullTable Whether to return a data frame including amplitude figures. 
#' If \code{TRUE}, a data frame with columns \code{Ch1.Amplitude}, 
#' \code{Ch1.Amplitude} and \code{class} is returned. If \code{FALSE}, a factor 
#' with levels in \code{ddpcr$classesRain} is returned, where each entry 
#' corresponds to each row in \code{droplets} (and \code{trainingData} is 
#' automatically set to \code{FALSE}). Defaults to \code{TRUE}.
#'
#' @return If \code{droplets} is a data frame, return a data frame or factor 
#' (depending on the \code{trainingData} and \code{fullTable} parameters) with 
#' a classification for droplets in the chosen regions.
#'
#' @exportMethod thresholdClassify

setMethod("thresholdClassify", "data.frame",
  function(droplets, ch1Threshold=6500, ch2Threshold=2900, fullTable=TRUE)
  {
    gridClassify(droplets,
                 ch1NNThreshold=ch1Threshold,
                 ch2NNThreshold=ch2Threshold,
                 ch1NPThreshold=ch1Threshold,
                 ch2NPThreshold=ch2Threshold,
                 ch1PNThreshold=ch1Threshold,
                 ch2PNThreshold=ch2Threshold,
                 ch1PPThreshold=ch1Threshold,
                 ch2PPThreshold=ch2Threshold,
                 trainingData=TRUE,
                 fullTable=fullTable)
  }
)


#' @rdname thresholdClassify
#'
#' @param classMethodLabel A name (as a character string) of the classification 
#' method. Defaults to "thresholds".
#'
#' @return If \code{droplets} is a \code{ddpcrWell} object, return 
#' a \code{ddpcrWell} object with the appropriate classification.
#'
#' @exportMethod thresholdClassify

setMethod("thresholdClassify", "ddpcrWell",
  function(droplets, ch1Threshold=6500, ch2Threshold=2900, 
           classMethodLabel="thresholds")
  {
    gridClassify(droplets,
                 ch1NNThreshold=ch1Threshold,
                 ch2NNThreshold=ch2Threshold,
                 ch1NPThreshold=ch1Threshold,
                 ch2NPThreshold=ch2Threshold,
                 ch1PNThreshold=ch1Threshold,
                 ch2PNThreshold=ch2Threshold,
                 ch1PPThreshold=ch1Threshold,
                 ch2PPThreshold=ch2Threshold,
                 classMethodLabel=classMethodLabel)
  }
)


#' @rdname thresholdClassify
#'
#' @return If \code{droplets} is a \code{ddpcrPlate} object, return 
#' a \code{ddpcrPlate} object with the appropriate classification.
#'
#' @exportMethod thresholdClassify

setMethod("thresholdClassify",
  "ddpcrPlate",
  function(droplets, ch1Threshold=6500, ch2Threshold=2900, 
           classMethodLabel="thresholds")
  {
    gridClassify(droplets,
                 ch1NNThreshold=ch1Threshold,
                 ch2NNThreshold=ch2Threshold,
                 ch1NPThreshold=ch1Threshold,
                 ch2NPThreshold=ch2Threshold,
                 ch1PNThreshold=ch1Threshold,
                 ch2PNThreshold=ch2Threshold,
                 ch1PPThreshold=ch1Threshold,
                 ch2PPThreshold=ch2Threshold,
                 classMethodLabel=classMethodLabel)
  }
)

