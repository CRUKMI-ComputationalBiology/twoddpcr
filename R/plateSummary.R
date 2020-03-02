#' @import methods
NULL

#' Round to at least n decimal places.
#'
#' For a data frame, if an extry is < 1, then round it to n significant
#' figures. If it is >= 1, then round it to n decimal places.
#'
#' @param df A data frame.
#' @param n How many decimal places/significant figures to round to.
#'
#' @return The data frame \code{x} with rounded entries.

.roundIt <- function(df, n=3)
{
  for(cn in colnames(df))
  {
    if(is.numeric(df[, cn]))
      df[, cn] <-
        ifelse(abs(df[, cn]) < 1, signif(df[, cn], n), round(df[, cn], n))
  }
  df
}


#' Get a vector of droplet positive and negative counts.
#'
#' Take a vector of classes and return a vector of positive and negative
#' counts that is compatible with ddPCR analysis.
#'
#' @param cl A vector of classes that correspond to droplet amplitude data. The
#' vector should only contain the values "PP", "PN", "NP" or "NN".
#'
#' @return A vector corresponding to "PP", "PN", "NP" and "NN".
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Take a data frame and make it into the right format.
#' aWell <- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#'
#' ## Count the number of droplets in each cluster.
#' positiveCounts(aWell$Cluster)
#'
#' @export

positiveCounts <- function(cl)
{
  ppCount <- sum(cl == ddpcr$pp)
  pnCount <- sum(cl == ddpcr$pn)
  npCount <- sum(cl == ddpcr$np)
  nnCount <- sum(cl == ddpcr$nn)

  data.frame("PP"=ppCount, "PN"=pnCount, "NP"=npCount, "NN"=nnCount)
}


#' Counts the number of positives and negatives in an experiment and
#' produces estimates for the number of molecules.
#'
#' Takes a collection of classified droplets, each corresponding to a well, and
#' produces a list of positive/negative counts and estimates of how many
#' molecules are in each well.
#'
#' @param wells Either a \code{\link{ddpcrPlate}} object or a list of data
#' frames, each of which comprises droplet amplitudes and their corresponding
#' classifications in a given well.
#' @param ... Other options depending on the type of \code{wells}.
#' @param ch1Label The prefix to use for the channel 1 target. Defaults to
#' "Mt".
#' @param ch2Label The prefix to use for the channel 2 target. Defaults to
#' "Wt".
#' @param sortByLetter If \code{TRUE}, the resulting data frame is sorted by
#' the letter in the well names first, e.g. "A02" comes before "B01". If
#' \code{FALSE}, the result is sorted by the numeric component of the well
#' names first, e.g. "B01" comes before "A02". Defaults to \code{FALSE}.
#'
#' @return A data frame with droplet counts and molecules number estimates for
#' each well.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Take a ddpcrPlate object and summarise its wells.
#' krasPlate <- ddpcrPlate(KRASdata)
#' plateSummary(krasPlate, cMethod="Cluster")
#'
#' @export

setGeneric("plateSummary",
  function(wells, ..., ch1Label="Mt", ch2Label="Wt", sortByLetter=FALSE)
  {
    standardGeneric("plateSummary")
  }
)


#' @rdname plateSummary
#'
#' @exportMethod plateSummary

setMethod("plateSummary", "list",
  function(wells, ch1Label="Mt", ch2Label="Wt", sortByLetter=FALSE)
  {
    # No wells; return an empty data frame.
    if(length(wells) == 0)
    {
      data.frame()
    }
    else # Molecule counts.
    {
      allCounts <- do.call(rbind, lapply(wells, positiveCounts))
      cs <- fullCountsSummary(
        data.frame(Well=names(wells), allCounts),
        ch1Label=ch1Label, ch2Label=ch2Label)

      # Sort the wells by letters or numbers first.
      cs <- sortDataFrame(cs, sortByLetter=sortByLetter)

      .roundIt(cs)
    }
  }
)


#' @rdname plateSummary
#'
#' @param cMethod The classification method to create a summary for.
#'
#' @exportMethod plateSummary

setMethod("plateSummary", "ddpcrPlate",
  function(wells, cMethod, ch1Label="Mt", ch2Label="Wt",
               sortByLetter=FALSE)
  {
    cl <- plateClassification(wells, cMethod=cMethod, withAmplitudes=TRUE)
    plateSummary(cl, ch1Label=ch1Label, ch2Label=ch2Label,
                 sortByLetter=sortByLetter)
  }
)

