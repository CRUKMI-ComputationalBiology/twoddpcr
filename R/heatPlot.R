#' @include global.R
#' @include themes.R
#' @include dropletPlot.R
#' @import methods
#' @import ggplot2
NULL

#' @title Draw a heat plot of the droplets.
#'
#' @description Using alpha transparency only, it is generally difficult to see
#' where droplets are truly distributed and concentrated. A heat (density) plot
#' gives a better illustration of this.
#'
#' @param droplets A data frame of droplet amplitudes, a \code{ggplot},
#' \code{ddpcrWell} or \code{ddpcrPlate} object.
#' @param ch1Label The label for the channel 1 target. Defaults to "Ch1
#' Amplitude".
#' @param ch2Label The label for the channel 2 target. Defaults to "Ch2
#' Amplitude".
#' @param binwidth The width of each hexagonal bin in the 2d heat (density)
#' plot. Defaults to 100.
#' @param plotLimits A list of 2-element vectors with names \code{x} and
#' \code{y}. These are used to fix the x and y limits of the plot, which is
#' especially useful for comparing plots. Defaults to \code{list(x=c(1000,
#' 9000), y=c(3000, 13500))}.
#'
#' @return A heat plot as a \code{\link[ggplot2]{ggplot}} object.
#'
#' @aliases densityPlot
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @references The nice log-scaled palette was achieved using
#' \url{http://www.everydayanalytics.ca/2014/09/5-ways-to-do-2d-histograms-in-r.html}
#'
#' @examples
#' ## Density plot of a data frame.
#' heatPlot(KRASdata[["E03"]])
#'
#' ## Density plot of a ddpcrWell object.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' heatPlot(aWell)
#'
#' ## Density plot of a ddpcrPlate object with an adjusted bin size.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' heatPlot(krasPlate, binwidth=50)
#'
#' @export

heatPlot <- function(droplets,
                     ch1Label="Ch1 Amplitude",
                     ch2Label="Ch2 Amplitude",
                     binwidth=100,
                     plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
  UseMethod("heatPlot")
}


#' @rdname heatPlot
#'
#' @exportMethod heatPlot

setGeneric("heatPlot")


#' @import hexbin
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales trans_format
#' @exportMethod heatPlot

heatPlot.gg <- function(droplets,
                        ch1Label="Ch1 Amplitude",
                        ch2Label="Ch2 Amplitude",
                        binwidth=100,
                        plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
  # Use a nice palette.
  rf <- grDevices::colorRampPalette(rev(brewer.pal(11, 'Spectral')))
  r <- rf(32)

  # Draw the heat plot with the colours on a log scale.
  p <-
    droplets + stat_binhex(binwidth=c(binwidth,binwidth)) +
    scale_fill_gradientn(name="# droplets", colours=r, trans="log",
      labels=trans_format("identity", function(x) round(x, 0)))

  # Label the axes.
  p <- p + ylab(ch1Label)
  p <- p + xlab(ch2Label)

  p + expand_limits(x=plotLimits$x, y=plotLimits$y) +
    whiteTheme(legendPosition=c(1, 1), legendJustification=c(1, 1))
}


#' @rdname heatPlot
#'
#' @exportMethod heatPlot

setMethod(
  "heatPlot", "data.frame",
  function(droplets, ch1Label="Ch1 Amplitude", ch2Label="Ch2 Amplitude",
           binwidth=100, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
    # Something to plot.
    if(nrow(droplets) > 0) {
      heatPlot(ggplot(droplets,
                      aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude")),
               ch1Label=ch1Label, ch2Label=ch2Label,
               binwidth=binwidth, plotLimits=plotLimits)
    } else {
      # Nothing to plot.
      dropletPlot(droplets,
                  ch1Label=ch1Label, ch2Label=ch2Label,
                  plotLimits=plotLimits)
    }
  }
)


#' @rdname heatPlot
#'
#' @exportMethod heatPlot

setMethod("heatPlot", "ddpcrWell",
  function(droplets, ch1Label="Ch1 Amplitude", ch2Label="Ch2 Amplitude",
           binwidth=100, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
    # Something to plot.
    if(numDroplets(droplets) > 0) {
      heatPlot(ggplot.well(droplets,
                           aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude")),
               ch1Label=ch1Label, ch2Label=ch2Label,
               binwidth=binwidth, plotLimits=plotLimits)
    } else {
      # Nothing to plot.
      dropletPlot(droplets,
                  ch1Label=ch1Label, ch2Label=ch2Label,
                  plotLimits=plotLimits)
    }
  }
)


#' @rdname heatPlot
#'
#' @exportMethod heatPlot

setMethod("heatPlot", "ddpcrPlate",
  function(droplets, ch1Label="Ch1 Amplitude", ch2Label="Ch2 Amplitude",
           binwidth=100, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))) {
    # Something to plot.
    if(sum(numDroplets(droplets)) > 0) {
      heatPlot(ggplot.plate(droplets,
                            aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude")),
               ch1Label=ch1Label, ch2Label=ch2Label,
               binwidth=binwidth, plotLimits=plotLimits)
    } else {
      # Nothing to plot.
      dropletPlot(droplets,
                  ch1Label=ch1Label, ch2Label=ch2Label,
                  plotLimits=plotLimits)
    }
  }
)
