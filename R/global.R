#' An environment for package variables.
#'
#' Stores default variables used in the package.
#'
#' @return An environment for storing package variables.
#'
#' @export
ddpcr <- new.env()

# Global class names.
ddpcr$nn <- "NN"
ddpcr$pn <- "PN"
ddpcr$np <- "NP"
ddpcr$pp <- "PP"
ddpcr$rain <- "Rain"
ddpcr$na <- "N/A"
ddpcr$classes <- c(ddpcr$nn, ddpcr$np, ddpcr$pn, ddpcr$pp)
ddpcr$classesRain <- c(ddpcr$classes, ddpcr$rain, ddpcr$na)

# Colour-blind-friendly palette.
ddpcr$dropColours <- c("#0072B2", "#CC79A7", "#009E73", "#E69F00",
                       "#999999", "#999999")
names(ddpcr$dropColours) <- ddpcr$classesRain
  
ddpcr$dropletVolume <- 0.00085   # This is the volume given by Bio-Rad.


#' Set the droplet volume in microlitres (ul).
#'
#' Change the volume of droplets used in the ddPCR experiment.
#'
#' @param volume The new volume of each droplet. Defaults to 0.00085, which is 
#' the default given by Bio-Rad.
#' 
#' @return Sets the internal droplet volume.
#' 
#' @aliases setDropVolume setDropletVol setDropVol
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Change the droplet volume.
#' ddpcr$dropletVolume
#' setDropletVolume(0.00091)
#' ddpcr$dropletVolume
#'
#' @export

setDropletVolume <- function(volume=0.00085)
{
  ddpcr$dropletVolume <- volume
}

