#' @include dropletPlot.R
#' @import methods
NULL

#' @title Plot droplet amplitudes with all droplets classified as "N/A" (or
#' a chosen class).
#'
#' @description There are occasions where classification algorithms fail for
#' various reasons (such as poor choice/number of centres in k-means
#' clustering). In these cases, it may be helpful to the user if an app draws
#' a 'flat' plot with just one colour rather than nothing at all.
#'
#' @param droplets A data frame of droplet amplitudes, or
#' a \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}} object.
#' @param ch1Label The label for the channel 1 target. Defaults to "Ch1
#' Amplitude".
#' @param ch2Label The label for the channel 2 target. Defaults to "Ch2
#' Amplitude".
#' @param classString The class that all droplets should be classified as.
#' Defaults to the \code{ddpcr$na} ("N/A") character string.
#' @param initialCentres A data frame of initial centres to plot (e.g. initial
#' cluster centres used in the k-means). This is _not_ restricted to the class
#' \code{classString} only. If \code{NULL}, nothing is plotted. Defaults to
#' \code{NULL}.
#' @param selectedCentre An initial centre to highlight. This should be either
#' "NN", "PN", "NP" or "PP", but is _not_ restricted to the class 'classString'
#' only. If \code{NULL}, nothing is highlighted. Defaults to \code{NULL}.
#' @param plotLimits A list of 2-element vectors with names \code{x} and
#' \code{y}. These are used to fix the x and y limits of the plot, which is
#' especially useful for comparing plots. Defaults to
#' \code{list(x=c(1000, 9000), y=c(3000, 13500))}.
#'
#' @return A ggplot object in just one colour corresponding to
#' \code{classString}.
#'
#' @name flatPlot
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Plot the data frame with no classification.
#' aWell <- KRASdata[["E03"]]
#' flatPlot(aWell)
#'
#' ## Take a ddpcrWell object that is mostly classified as "NN" and colour it
#' ## as such.
#' aWell <- KRASdata[["H04"]]
#' emptiedWell <- aWell[aWell$Cluster == 1, ]
#' emptiedWell <- ddpcrWell(well=emptiedWell)
#' flatPlot(emptiedWell, classString="NN")
#'
#' ## Plotting all of a ddpcrPlate object works the same way.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' flatPlot(krasPlate)
#'
#' @export

flatPlot <- function(droplets,
                     ch1Label="Ch1 Amplitude",
                     ch2Label="Ch2 Amplitude",
                     classString=ddpcr$na,
                     initialCentres=NULL, selectedCentre=NULL,
                     plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)))
  UseMethod("flatPlot")


#' @export

setGeneric("flatPlot")


#' @rdname flatPlot
#'
#' @exportMethod flatPlot

setMethod("flatPlot", "data.frame",
  function(droplets,
           ch1Label="Ch1 Amplitude",
           ch2Label="Ch2 Amplitude",
           classString=ddpcr$na,
           initialCentres=NULL, selectedCentre=NULL,
           plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
    if(nrow(droplets) > 0) {
      droplets <- data.frame(droplets[, c("Ch1.Amplitude", "Ch2.Amplitude")],
                             "class"=classString)
    }
    dropletPlot(droplets, cMethod="class", initialCentres=initialCentres,
                selectedCentre=selectedCentre, plotLimits=plotLimits)
  }
)


#' @rdname flatPlot
#'
#' @description If a \code{ddpcrWell} object is given as a parameter, plot the
#' droplets in the well and colour them according to a given class.
#'
#' @exportMethod flatPlot

setMethod("flatPlot", "ddpcrWell",
  function(droplets,
           ch1Label="Ch1 Amplitude",
           ch2Label="Ch2 Amplitude",
           classString=ddpcr$na,
           initialCentres=NULL, selectedCentre=NULL,
           plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
    if(!isEmpty(droplets)) {
      droplets <- data.frame(amplitudes(droplets), class=classString)
    }
    dropletPlot(droplets, cMethod="class", initialCentres=initialCentres,
                selectedCentre=selectedCentre, plotLimits=plotLimits)
  }
)


#' @rdname flatPlot
#'
#' @description If a \code{ddpcrPlate} object is given as a parameter,
#' plot the droplets in all wells and colour them according to a given class.
#'
#' @exportMethod flatPlot

setMethod("flatPlot", "ddpcrPlate",
  function(droplets,
           ch1Label="Ch1 Amplitude",
           ch2Label="Ch2 Amplitude",
           classString=ddpcr$na,
           initialCentres=NULL, selectedCentre=NULL,
           plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
    if(!isEmpty(droplets)) {
      droplets <- do.call(rbind, amplitudes(droplets))
      droplets <- data.frame(droplets, class=classString)
    }
    dropletPlot(droplets, cMethod="class", initialCentres=initialCentres,
                selectedCentre=selectedCentre, plotLimits=plotLimits)
  }
)
