#' Return given well names sorted.
#'
#' Well names can be sorted according to the leading letter or the trailing
#' digit.
#'
#' @param wellNames The well names to sort.
#' @param sortByLetter If \code{TRUE}, the resulting list is sorted by the
#' letter in the well names first, e.g. "A02" comes before "B01". If
#' \code{FALSE}, the result is sorted by the numeric component of the well
#' names first, e.g. "B01" comes before "A02". Defaults to \code{FALSE}.
#'
#' @return The well names sorted.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Use the KRASdata dataset as an example.
#' plate <- ddpcrPlate(wells=KRASdata)
#' wellNames <- names(plate)
#'
#' ## Sample sorting.
#' sortWells(wellNames=wellNames, sortByLetter=FALSE)
#' sortWells(wellNames=wellNames, sortByLetter=TRUE)
#'
#' @export sortWells

sortWells <- function(wellNames, sortByLetter) {
  if(sortByLetter) {
    wellNames[order(wellNames)]
  } else {
    wellNames[order(substr(wellNames, start=2, stop=3),
                    substr(wellNames, start=1, stop=1))]
  }
}


#' Sorts a data frame according to the well names.
#'
#' Well names can be sorted according to the leading letter or the trailing
#' digit.
#'
#' @param df A data frame with row names of the form A01, A02, B01, etc.
#' @param sortByLetter If \code{TRUE}, the resulting list is sorted by the
#' letter in the well names first, e.g. "A02" comes before "B01". If
#' \code{FALSE}, the result is sorted by the numeric component of the well
#' names first, e.g. "B01" comes before "A02". Defaults to \code{FALSE}.
#'
#' @return The data frame \code{df} with the rows sorted.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Sort a subset of the KRAScounts data frame.
#' df <- KRAScounts[, c("Ratio", "FracAbun")]
#' sortDataFrame(df=df, sortByLetter=FALSE)
#' sortDataFrame(df=df, sortByLetter=TRUE)
#'
#' @export sortDataFrame

sortDataFrame <- function(df, sortByLetter=FALSE) {
  if(sortByLetter) {
    df[order(rownames(df)), ]
  } else {
    df[order(substr(rownames(df), start=2, stop=3),
             substr(rownames(df), start=1, stop=1)), ]
  }
}
