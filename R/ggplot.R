#' @import methods
#' @import ggplot2
NULL

#' @title ggplot methods for the \code{\link{ddpcrWell}} and
#' \code{\link{ddpcrPlate}} classes.
#'
#' @description These functions work in the same way as the original
#' \code{\link[ggplot2]{ggplot}} method, but handles the coercion of the object
#' into a data frame.
#'
#' @param data A \code{ddpcrWell} or \code{ddpcrPlate} object.
#' @param mapping A list of aesthetic mappings to use for the plot. Defaults to
#' \code{ggplot2::aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude",
#' colour=cMethod)}, where \code{cMethod} is taken from the parameter of the
#' same name.
#' @param cMethod The name or column number of the classification to use. This
#' is renamed internally to "class" for use with \code{mapping}. Defaults to
#' "None".
#' @param ... Other arguments passed onto \code{\link[ggplot2]{ggplot}}.
#' @param environment Where to look if a mapping variable is not defined.
#' Defaults to \code{parent.frame()}, i.e. the environment in which
#' \code{ggplot.well()} or \code{ggplot.multiwell()} is called.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object using the slots in the given
#' object.
#'
#' @name ggplot.well
#'
#' @aliases ggplot.well ggplot.wells ggplot.plate ggplot.multiwell
#'
#' @seealso \code{\link{dropletPlot}} builds upon these \code{ggplot} methods
#' to plot droplet amplitude plots with a colour-blind friendly palette.
#' @seealso The original \code{\link[ggplot2]{ggplot}} method in the
#' \code{ggplot2} package is used internally.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Plot the droplets in one well.
#' library(ggplot2)
#' aWell <- ddpcrWell(KRASdata[["E03"]])
#' ggplot.well(aWell, cMethod="Cluster") + geom_point()
#'
#' ## Plot the droplets in all of the wells in a single plot.
#' krasPlate <- ddpcrPlate(KRASdata)
#' ggplot.plate(krasPlate, cMethod="Cluster") + geom_point()
#'
#' @export

setGeneric(
  "ggplot.well",
  function(data, mapping=aes_string(x="Ch2.Amplitude",
                                    y="Ch1.Amplitude",
                                    colour=cMethod),
           cMethod=NULL, ..., environment=parent.frame()) {
    standardGeneric("ggplot.well")
  }
)

#' @rdname ggplot.well
#'
#' @description \code{ggplot.well} is a \code{\link[ggplot2]{ggplot}} method
#' for the \code{\link{ddpcrWell}} class.
#'
#' @exportMethod ggplot.well

setMethod("ggplot.well", "ddpcrWell",
  function(data, mapping=aes_string(x="Ch2.Amplitude",
                                    y="Ch1.Amplitude",
                                    colour=cMethod),
           cMethod="None", ..., environment=parent.frame()) {
    if(numDroplets(data) == 0) {
      # Nothing to plot. To stop things breaking, remove references to
      # a meaningless 'colour' aesthetic.
      mapping <- mapping[Filter(Negate(function(k) { k == "colour" }),
                                names(as.list(mapping)))]
      ggplot(data.frame("Ch1.Amplitude"=double(), "Ch2.Amplitude"=double()),
             mapping, ...)
    } else {
      # Standard ggplot object.
      data <- wellClassification(data, cMethod=cMethod, withAmplitudes=TRUE)
      ggplot(data, mapping, ...)
    }
  }
)


#' @rdname ggplot.well
#'
#' @export

setGeneric(
  "ggplot.plate",
  function(data, mapping=aes_string(x="Ch2.Amplitude",
                                    y="Ch1.Amplitude",
                                    colour=class),
           cMethod="None", ..., environment=parent.frame()) {
    standardGeneric("ggplot.plate")
  }
)

#' @rdname ggplot.well
#'
#' @description \code{ggplot.multiwell} is a \code{\link[ggplot2]{ggplot}}
#' method for the \code{\link{ddpcrPlate}} class.
#'
#' @exportMethod ggplot.plate

setMethod(
  "ggplot.plate", "ddpcrPlate",
  function(data, mapping=aes_string(x="Ch2.Amplitude",
                                    y="Ch1.Amplitude",
                                    colour=cMethod),
           cMethod="None", ..., environment=parent.frame()) {
    if(sum(numDroplets(data)) == 0) {
      # Remove references to colour.
      mapping <- mapping[Filter(Negate(function(k) { k == "colour" }),
                                names(as.list(mapping)))]
      ggplot(data.frame("Ch1.Amplitude"=double(), "Ch2.Amplitude"=double()),
             mapping, ...)
    } else {
      data <- plateClassification(data, cMethod=cMethod, withAmplitudes=TRUE)
      data <- do.call(rbind, data)
      ggplot(data, mapping, ...)
    }
  }
)
