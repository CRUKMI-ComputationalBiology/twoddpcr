#' @include global.R
#' @include themes.R
#' @import ggplot2
#' @import methods
NULL

#' Plot nothing.
#'
#' We may sometimes request that the \code{\link{dropletPlot}} method takes an 
#' empty data frame as its argument, e.g. an empty CSV file is loaded. This 
#' normally presents an error, which is not the desired output. This function 
#' plots nothing to show that there was no data to plot.
#'
#' @seealso \code{\link{ggplot.well}} and 
#' \code{\link[=ggplot.well]{ggplot.plate}} are wrappers for plotting 
#' \code{\link{ddpcrWell}} and \code{\link{ddpcrPlate}} objects.
#' @seealso If there is at least one droplet, the \code{\link{dropletPlot}} 
#' method plots droplet amplitude classifications.
#'
#' @return A blank \code{ggplot} object.
#'

drawBlank <- function()
{
  ggplot() + geom_blank() +
    whiteTheme(legendPosition=c(1, 1), legendJustification=c(1, 1))
}


# Use an S3 generic because the default class for 'droplets' is a 'gg' object, 
# which is not an S4 class.

#' @title Plot a droplet classification with a colour-blind palette, optional 
#' cluster centres and fixed axes.
#'
#' @description Plot an object comprising droplet amplitudes and their 
#' classification. If specified, centres of clusters can be marked, e.g. 
#' k-means clustering can take a set of centres as the initial centres of the 
#' algorithm, and the algorithm also outputs the final cluster centres. Limits 
#' to the axes can also be set for ease of comparison and consistency.
#'
#' If a \code{ggplot} object is given as a parameter, this method will simply 
#' plot it with the pretty colours, centres and restrictions on the axes.
#'
#' @param droplets An object corresponding to droplet amplitudes and their 
#' classifications. This can be in the form of:
#' \itemize{
#'   \item A data frame with columns \code{Ch1.Amplitude}, \code{Ch2.Amplitude} 
#'   and a classification column (see the parameter \code{cMethod}).
#'   \item A \code{\link{ddpcrWell}} object.
#'   \item A \code{\link{ddpcrPlate}} object.
#'   \item A \code{\link[ggplot2]{ggplot}} (\code{gg}) object. For example, 
#'   this could be the output of \code{\link[twoddpcr]{ggplot.well}} or 
#'   \code{\link[=ggplot.well]{ggplot.plate}}. We should not need to use this 
#'   unless we are writing new methods to plot new data types.
#' }
#' @param ch1Label The label for the channel 1 target. Defaults to "Ch1 
#' Amplitude".
#' @param ch2Label The label for the channel 2 target. Defaults to "Ch2 
#' Amplitude".
#' @param ... Other plotting parameters that depend on the object type of 
#' \code{droplets}.
#' @param cMethod This should be the name or column number of \code{droplets} 
#' corresponding to the classification. This column should only have entries in 
#' "NN", "PN", "NP, "PP", "Rain" and "N/A". If "None", plots the droplets with 
#' all of them classified as \code{N/A}. Defaults to "None".
#' @param mapping A list of aesthetic mappings to use for the plot. Defaults to 
#' \code{ggplot2::aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude", 
#' colours=cMethod)}. Not used if \code{droplets} is a \code{ggplot} object.
#' @param finalCentres A data frmae of final centres to plot (e.g. those
#' returned by the k-means or c-means algorithms). If \code{NULL}, nothing is 
#' plotted. Defaults to \code{NULL}.
#' @param initialCentres A data frame of initial centres to plot (e.g. initial
#' cluster centres used in the k-means). If \code{NULL}, nothing is plotted. 
#' Defaults to \code{NULL}. This parameter is useful for illustrative reasons.
#' @param selectedCentre An initial centre to highlight. This should be either
#' "NN", "NP", "PN" or "PP". If \code{NULL}, nothing is highlighted. Defaults 
#' to \code{NULL}. This parameter is useful for illustrative reasons.
#' @param pointSize The size to draw each droplet. Defaults to 1.
#' @param plotLimits A list of 2-element vectors with names \code{x} and 
#' \code{y}. These are used to fix the x and y limits of the plot, which is 
#' especially useful for comparing plots. Defaults to \code{list(x=c(1000, 
#' 9000), y=c(3000, 13500))}.
#' @param legendLabels The character vector corresponding to the labels for the 
#' legend. The elements of the vector should correspond to the NN, NP, PN, PP, 
#' Rain and N/A classes, respectively. Defaults to \code{ddpcr$classesRain}.
#'
#' @return A \code{ggplot} object with all of the given information above.
#'
#' @name dropletPlot
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get a data frame and relabel the "Cluster" column to the right form.
#' aWell <- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#'
#' ## Plot the data frame.
#' dropletPlot(aWell, cMethod="Cluster")
#'
#' ## Plot a ddpcrWell object.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' dropletPlot(aWell, cMethod="Cluster")
#'
#' ## Plot a ddpcrPlate object.
#' krasPlate <- ddpcrPlate(wells=KRASdata[c("E03", "H03", "C04", "F04")])
#' dropletPlot(krasPlate, cMethod="Cluster")
#'
#' ## Use K-means clustering to classify a single sample. Then plot the
#' ## classification and final cluster centres.
#' aWell <- kmeansClassify(aWell)
#' centres <- clusterCentres(aWell, cMethod="kmeans")
#' dropletPlot(aWell, cMethod="kmeans", finalCentres=centres)
#'
#' @export

dropletPlot <- function(droplets,
                        ch1Label="Ch1 Amplitude",
                        ch2Label="Ch2 Amplitude",
                        ...)
{
  UseMethod("dropletPlot")
}


# Still need to set an S4 generic for S4 classes.

#' @export

setGeneric("dropletPlot")


# This is an S3 method because 'gg' is an S3 class.

#' @exportMethod dropletPlot

dropletPlot.gg <- function(droplets,
                           ch1Label="Ch1 Amplitude",
                           ch2Label="Ch2 Amplitude",
                           finalCentres=NULL, initialCentres=NULL,
                           selectedCentre=NULL,
                           pointSize=1,
                           plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
                           legendLabels=ddpcr$classesRain)
{
  p <- droplets +
    geom_point(alpha=0.3, size=pointSize) +
    scale_colour_manual(values=ddpcr$dropColours, labels=legendLabels)

  # Plot the final centres.
  if(!is.null(finalCentres))
  {
    p <- p + geom_point(data=finalCentres,
                        fill="#000000",
                        colour="#FFFFFF",
                        alpha=0.8,
                        shape=9,
                        size=6)
  }

  # Plot the initial centres, highlighting any selected ones.
  if(!is.null(initialCentres))
  {
    if(is.null(selectedCentre))
      selectedCentre <- ddpcr$na
    p <- p +
      geom_point(
        data=initialCentres[rownames(initialCentres)==selectedCentre, ],
        fill="#FFFFFF", colour="#000000", alpha=0.8, shape=23, size=6) +
      geom_point(
        data=initialCentres[rownames(initialCentres)!=selectedCentre, ],
        fill="#000000", colour="#FFFFFF", alpha=0.8, shape=23, size=6)
  }

  # Label the axes.
  p <- p + ylab(ch1Label)
  p <- p + xlab(ch2Label)
      
  # Standardise the limits to make it easy to compare plots.
  p + expand_limits(x=plotLimits$x, y=plotLimits$y) +
    whiteTheme(legendPosition=c(1, 1), legendJustification=c(1, 1))
}


#' @description If a \code{data.frame} is given as a parameter, it should 
#' correspond to droplets with their classification.
#'
#' @rdname dropletPlot
#'
#' @exportMethod dropletPlot

setMethod("dropletPlot", "data.frame",
  function(droplets,
           ch1Label="Ch1 Amplitude",
           ch2Label="Ch2 Amplitude",
           cMethod="None",
           mapping=aes_string(x="Ch2.Amplitude",
                              y="Ch1.Amplitude",
                              colour=cMethod),
           finalCentres=NULL, initialCentres=NULL, selectedCentre=NULL,
           pointSize=1, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
           legendLabels=ddpcr$classesRain)

  {
    if(nrow(droplets) > 0)
    {
      # Add a dummy column if needed.
      if(cMethod == "None" && !"None" %in% colnames(droplets))
        droplets$None <- factor(c("N/A"), levels=ddpcr$classesRain)
      
      # Get the right ggplot object.
      p <- ggplot(droplets, mapping=mapping)
    }
    else # Nothing to draw.
      p <- drawBlank()

    dropletPlot(p,
                ch1Label=ch1Label, ch2Label=ch2Label,
                finalCentres=finalCentres,
                initialCentres=initialCentres,
                pointSize=pointSize,
                plotLimits=plotLimits,
                legendLabels=legendLabels)
  }
)


#' @description If a \code{ddpcrWell} object is given as a parameter, plot the 
#' droplets in the well with its classification.
#'
#' @rdname dropletPlot
#'
#' @exportMethod dropletPlot

setMethod("dropletPlot", "ddpcrWell",
  function(droplets,
           ch1Label="Ch1 Amplitude",
           ch2Label="Ch2 Amplitude",
           cMethod="None",
           mapping=aes_string(x="Ch2.Amplitude",
                              y="Ch1.Amplitude",
                              colour=cMethod),
           finalCentres=NULL, initialCentres=NULL, selectedCentre=NULL,
           pointSize=1, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
           legendLabels=ddpcr$classesRain)
  {
    p <- ggplot.well(droplets, cMethod=cMethod, mapping=mapping)
    dropletPlot(p,
                ch1Label=ch1Label, ch2Label=ch2Label,
                finalCentres=finalCentres,
                initialCentres=initialCentres,
                pointSize=pointSize,
                plotLimits=plotLimits,
                legendLabels=legendLabels)
  }
)


#' @rdname dropletPlot

#' @description If a \code{ddpcrPlate} object is given as a parameter, plot the 
#' droplets from all wells with their classifications.
#'
#' @exportMethod dropletPlot

setMethod("dropletPlot", "ddpcrPlate",
  function(droplets,
           ch1Label="Ch1 Amplitude",
           ch2Label="Ch2 Amplitude",
           cMethod="None",
           mapping=aes_string(x="Ch2.Amplitude",
                              y="Ch1.Amplitude",
                              colour=cMethod),
           finalCentres=NULL, initialCentres=NULL, selectedCentre=NULL,
           pointSize=1, plotLimits=list(x=c(1000, 9000), y=c(3000, 13500)),
           legendLabels=ddpcr$classesRain)
  {
    p <- ggplot.plate(droplets, cMethod=cMethod, mapping=mapping)
    dropletPlot(p, ch1Label=ch1Label, ch2Label=ch2Label,
                finalCentres=finalCentres,
                initialCentres=initialCentres,
                pointSize=pointSize,
                plotLimits=plotLimits,
                legendLabels=legendLabels)
  }
)

