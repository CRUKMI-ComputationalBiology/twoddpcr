#' @include global.R
#' @include themes.R
#' @include dropletPlot.R
#' @import methods
#' @import ggplot2
#' @import hexbin
NULL

#' @title Draw each of the individual wells in a ddPCR experiment.
#'
#' @description Plot each of the wells in a \code{\link{ddpcrPlate}} 
#' object or a large data frame of droplets. Use a density plot for speed.
#'
#' @param droplets A \code{ddpcrPlate} object or a data frame of droplet 
#' amplitudes with a "Well" column.
#' @param ch1Label The label for the channel 1 target. Defaults to "Ch1 
#' Amplitude".
#' @param ch2Label The label for the channel 2 target. Defaults to "Ch2 
#' Amplitude".
#' @param binwidth The width of each hexagonal bin in the density plot. 
#' Defaults to 100.
#' @param plotLimits A list of 2-element vectors with names \code{x} and 
#' \code{y}. These are used to fix the x and y limits of the plot, which is 
#' especially useful for comparing plots. Defaults to \code{list(x=c(1000, 
#' 9000), y=c(3000, 13500))}.
#' @param showEmptyWells If \code{TRUE}, plots 
#' a \code{\link[ggplot2]{facet_grid}} of all the wells in the plate, including 
#' the empty ones. If \code{FALSE}, plots a \code{\link[ggplot2]{facet_wrap}} 
#' of only the loaded (nonempty) wells. Defaults to \code{FALSE}.
#'
#' @return A collection of heat plots as a \code{\link[ggplot2]{ggplot}} 
#' object.
#'
#' @aliases allPlot plotAll
#'
#' @seealso Each subplot uses the same plotting style as 
#' \code{\link{heatPlot}}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @references The nice log-scaled palette was achieved using 
#' \url{http://www.everydayanalytics.ca/2014/09/5-ways-to-do-2d-histograms-in-r.html}
#'
#' @examples
#' ## Plot a facet wrap of density plots of each well.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' facetPlot(krasPlate)
#'
#' @export

setGeneric("facetPlot",
  function(droplets, ch1Label="Ch1 Amplitude", ch2Label="Ch2 Amplitude",
           binwidth=100, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
           showEmptyWells=FALSE)
  {
    standardGeneric("facetPlot")
  }
)


#' @rdname facetPlot
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales trans_format
#' @exportMethod facetPlot

setMethod("facetPlot", "data.frame",
  function(droplets,
           ch1Label="Ch1 Amplitude", ch2Label="Ch2 Amplitude",
           binwidth=100,
           plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
           showEmptyWells=FALSE)
  {
    if(nrow(droplets) > 0)
    {
      droplets$WellLetter <-
        factor(substr(droplets$Well, start=1, stop=1), levels=LETTERS[1:8])
      droplets$WellDigit <-
        factor(substr(droplets$Well, start=2, stop=3),
               levels=formatC(1:12, width=2, format="d", flag="0"))
      
      # Use a nice palette.
      rf <- grDevices::colorRampPalette(
        rev(RColorBrewer::brewer.pal(11, 'Spectral')))
      r <- rf(32)

      # Draw the heat plot with the colours on a log scale.
      p <-
        ggplot(droplets, aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude")) +
          stat_binhex(binwidth=c(binwidth,binwidth)) +
          scale_fill_gradientn(
            name="# droplets", colours=r, trans="log",
            labels=scales::trans_format(
              "identity", function(x) round(x, 0))) +
          expand_limits(x=plotLimits$x, y=plotLimits$y)
      
      # Display all of the wells or just the nonempty ones.
      if(showEmptyWells)
        p <- p + facet_grid(WellLetter ~ WellDigit, drop=FALSE)
      else
        p <- p + facet_wrap(~ Well)
      
      # Label the axes.
      p <- p + ylab(ch1Label)
      p <- p + xlab(ch2Label)
      
      # Theme options.
      p + whiteTheme() +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
    }
    else
      dropletPlot(droplets, ch1Label=ch1Label, ch2Label=ch2Label, 
                  plotLimits=plotLimits)
  }
)

#' @rdname facetPlot
#'
#' @exportMethod facetPlot

setMethod("facetPlot", "ddpcrPlate",
  function(droplets,
           ch1Label="Ch1 Amplitude", ch2Label="Ch2 Amplitude",
           binwidth=100,
           plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
           showEmptyWells=FALSE)
  {
    if(sum(numDroplets(droplets)) > 0)
    {
      cl <- do.call(rbind, plateClassification(droplets, wellCol=TRUE))
      facetPlot(cl,
                ch1Label=ch1Label, ch2Label=ch2Label,
                binwidth=binwidth, plotLimits=plotLimits,
                showEmptyWells=showEmptyWells)
    }
    else
      dropletPlot(droplets, ch1Label=ch1Label, ch2Label=ch2Label,
                  plotLimits=plotLimits)
  }
)

