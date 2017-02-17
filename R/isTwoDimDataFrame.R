#' @import methods
NULL

#' Checks whether an object is a data frame with two leading double columns.
#'
#' @param df A data frame.
#'
#' @return A logical value: whether the data frame meets the criteria.

.isTwoDimDataFrame <- function(df)
{
  return(is.data.frame(df) && ncol(df) >= 2 &&
         is.numeric(df[, 1]) && is.numeric(df[, 2]))
}

#' Rename the channels.
#'
#' We use the channel names "Ch1.Amplitude" and "Ch2.Amplitude" by default. 
#' This method allows us to change it at will.
#'
#' @param droplets A data frame or list of data frames. Each data frame should 
#' have at least two columns, where the first two columns should be vectors of 
#' doubles.
#' @param ch1 The channel 1 label.
#' @param ch2 The channel 2 label.
#'
#' @return The object \code{droplets} with the channels renamed.
#'
#' @name setChannelNames
#'
#' @examples
#' ## Ensure that a data frame has channels named "Ch1.Amplitude" and
#' ## "Ch2.Amplitude".
#' aWell <- KRASdata[["E03"]]
#' aWell <- setChannelNames(aWell)
#' colnames(aWell)
#'
#' ## Perhaps we want to abbreviate the channel names.
#' aWell <- setChannelNames(aWell, "Ch1", "Ch2")
#' colnames(aWell)
#'
#' ## The same operator works for a list of data frames.
#' krasWells <- KRASdata
#' krasWells <- setChannelNames(krasWells, "Ch1", "Ch2")
#' lapply(krasWells, colnames)[1:3]
#'
#' @export

setGeneric("setChannelNames",
  function(droplets, ch1="Ch1.Amplitude", ch2="Ch2.Amplitude")
  {
    standardGeneric("setChannelNames")
  }
)


#' @rdname setChannelNames
#'
#' @exportMethod setChannelNames

setMethod("setChannelNames", "data.frame",
  function(droplets, ch1="Ch1.Amplitude", ch2="Ch2.Amplitude")
  {
    if(.isTwoDimDataFrame(droplets))
    {
      colnames(droplets)[1:2] <- c(ch1, ch2)
      droplets
    }
    else
      stop("Not all data frames represent 2-d droplet amplitudes.")
  }
)


#' @rdname setChannelNames
#'
#' @exportMethod setChannelNames

setMethod("setChannelNames", "list",
  function(droplets, ch1="Ch1.Amplitude", ch2="Ch2.Amplitude")
  {
    if(all(vapply(droplets, class, character(1)) == "data.frame"))
    {
      lapply(droplets, setChannelNames, ch1=ch1, ch2=ch2)
    }
    else
      stop("Not all of the list elements are data frames.")
  }
)

