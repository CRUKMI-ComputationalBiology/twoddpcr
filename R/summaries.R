#' Get the very basic columns of a data frame.
#'
#' It is useful to see the droplet counts for each class in addition to the
#' overall droplet count at a glance.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with the \code{PP}, \code{PN}, \code{NP}, \code{NN} and
#' \code{AcceptedDroplets} figures.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' df <- fullCountsSummary(KRAScountsQS)
#' basicsSummary(df)
#'
#' @export

basicsSummary <- function(df) {
  df[, c("PP", "PN", "NP", "NN", "AcceptedDroplets")]
}


#' Get the mutant copies per 20ul of a data frame.
#'
#' Returns a data frame with the number of mutant copies per 20ul.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with the basic columns and mutant copies per 20ul.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.getMutCopies <- function(df) {
  data.frame("MUT"=c("MUT"), "Copies/20ul"=df$MtCopiesPer20uLWell,
             check.names=FALSE)
}


#' Get the mutant copies per 20ul of a data frame in the context of the
#' basic counts.
#'
#' Returns a data frame with the basic figures and the number of mutant copies
#' per 20ul.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with the basic columns and mutant copies per 20ul.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' df <- fullCountsSummary(KRAScountsQS)
#' mutantCopiesSummary(df)
#'
#' @export

mutantCopiesSummary <- function(df) {
  if(!.isWideForm(df)) {
    stop("Input data frame is of the wrong format. ",
         "Try castMtWt first.")
  }
  data.frame(basicsSummary(df), .getMutCopies(df), check.names=FALSE)
}


#' Get the wild type copies per 20ul of a data frame.
#'
#' Returns a data frame with the number of wild type copies per 20ul.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with the basic columns and wild type copies per 20ul.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.getWtCopies <- function(df) {
  data.frame("WT"=c("WT"), "Copies/20ul"=df$WtCopiesPer20uLWell,
             check.names=FALSE)
}


#' Get the wild type copies per 20ul of a data frame in the context of the
#' basic counts.
#'
#' Returns a data frame with the basic figures and the number of wild type
#' copies per 20ul.
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with the basic columns and wild type copies per 20ul.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' df <- fullCountsSummary(KRAScountsQS)
#' wildTypeCopiesSummary(df)
#'
#' @export

wildTypeCopiesSummary <- function(df) {
  if(!.isWideForm(df)) {
    stop("Input data frame is of the wrong format. ",
         "Try castMtWt first.")
  }
  data.frame(basicsSummary(df), .getWtCopies(df), check.names=FALSE)
}


#' Get the total number of molecules in 20ul.
#'
#' Get the total number of copies of mutant and wild type molecules in a 20ul
#' well.
#'
#' @param df An input data frame with copies per 20ul figures.
#'
#' @return A vector listing the total numbers of copies per 20ul well.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.totalCopies <- function(df) {
  if(is.null(df$TotalCopiesPer20uLWell)) {
    df$MtCopiesPer20uLWell + df$WtCopiesPer20uLWell
  } else {
    df$TotalCopiesPer20uLWell
  }
}


#' Get a summary of the number of molecules in 20ul.
#'
#' Returns a data frame with a summary of the number of mutant and wild type
#' copies per 20ul.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with mutant and wild type copies per 20ul, the total
#' number of copies per 20ul, and a flag indicating if there are more than
#' 2 mutant copies per 20ul.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.getAllSummary <- function(df) {
  data.frame("MA Calc"=c("MA Calc"),
             "MUT Copies/20ul"=df$MtCopiesPer20uLWell,
             "WT Copies/20ul"=df$WtCopiesPer20uLWell,
             "Total Copies/20ul"=.totalCopies(df),
             "%MUT"=df$FracAbun,
             "MUT Copies > 2"=ifelse(df$MtCopiesPer20uLWell>2, 1, 0),
             check.names=FALSE)
}


#' Get the total copies per 20ul of a data frame in the context of the
#' basic counts.
#'
#' Returns a data frame with the basic figures and a summary of the number of
#' copies per 20ul.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#'
#' @return A data frame with the basic data columns, mutant and wild type
#' copies per 20ul, the total number of copies per 20ul, and a flag indicating
#' if there are more than 2 mutant copies per 20ul.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' df <- fullCountsSummary(KRAScountsQS)
#' copiesSummary(df)
#'
#' @export

copiesSummary <- function(df) {
  if(!.isWideForm(df)) {
    stop("Input data frame is of the wrong format. ",
         "Try castMtWt first.")
  }
  data.frame(basicsSummary(df), .getAllSummary(df), check.names=FALSE)
}


#' Get all of the counts data in a summarised data frame.
#'
#' Returns a data frame with all the copies information, plus any optional
#' columns. This function is intended to be used as a final summary of the
#' molecule counts.
#'
#' @param df A data frame generated by \code{\link{fullCountsSummary}}.
#' @param extraCols A vector of column names from \code{df} to include. If
#' \code{NULL}, no extra columns are added. Defaults to \code{NULL}.
#'
#' @return A data frame with the basic figures, the mutant counts, the wild
#' type counts, the summarised counts, and extraCols if specified. Prints an
#' additional column for notes, indicating whether this run failed or if there
#' were fewer than 8000 accepted droplets.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' df <- fullCountsSummary(KRAScountsQS)
#' fullCopiesSummary(df)
#'
#' @export

fullCopiesSummary <- function(df, extraCols=NULL) {
  if(!.isWideForm(df)) {
    stop("Input data frame is of the wrong format. ",
         "Try castMtWt first.")
  }

  # Build a data frame with the details of the copies plus extra columns.
  d <- data.frame(basicsSummary(df), .getMutCopies(df), .getWtCopies(df),
                  .getAllSummary(df), check.names=FALSE)

  # Include any extra columns.
  if(!is.null(extraCols)) {
    d <- data.frame(d,
                    df[, extraCols],
                    "Total Copies/20ul"=.totalCopies(df),
                    check.names=FALSE)
  }

  # Add some notes to the end.
  d$Notes <- ifelse(is.na(df$AcceptedDroplets),
               "Not done",
             ifelse(df$AcceptedDroplet == 0,
               "Failed",
             ifelse(df$AcceptedDroplets <= 8000,
               "<= 8000 droplets",
             "None")))
  d
}
