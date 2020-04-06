#' Find the number of wells in the data frame.
#'
#' Auxiliary function to find the number of wells that appear in the data
#' frame, each with the same frequency (almost certainly once or twice). If
#' some wells appear more than others, the function stops because the data is
#' in an unknown format.
#'
#' Bio-Rad's QuantaSoft produces data in long form tables, i.e. for each well
#' there is a row for each target such as mutant and wild type. In this
#' package, we prefer to work with wide form tables that combine the mutant and
#' wild type parts.
#'
#' @param df A data frame of droplet counts with a \code{Well} column.
#'
#' @return An integer giving the number of wells.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.numberOfWells <- function(df) {
  if(nrow(df) == 0) {
    stop("Input data frame has zero rows.")
  }

  # Assume that the row names are the well names, and hence none are repeated.
  if(!"Well" %in% colnames(df)) {
    return(nrow(df))
  }

  # Check that each well appears the same number of times in df.
  wellOccurances <- unique(table(as.character(df$Well)))
  if(length(wellOccurances) == 1) {
    return(nrow(df) / wellOccurances[1])
  } else {
    stop("Some wells appear more than others in the data frame.")
  }
}


#' Extract the well names from a data frame.
#'
#' Get the well names from a data frame, checking if there is a \code{Well}
#' column or by using the row numbering.
#'
#' @param df A data frame.
#'
#' @return A factor of row names.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.getWellNames <- function(df) {
  if("Well" %in% colnames(df)) {
    return(df$Well)
  } else {
    return(rownames(df))
  }
}


#' Retain cluster counts and user-specified columns in data frames.
#'
#' Take a data frame of droplet counts and returns only the raw "PP", "PN",
#' "NP" and "NN" counts, plus any additional columns specified.
#'
#' @param df A data frame with droplet count columns in one of the following
#' formats:
#' \itemize{
#'   \item{\code{PP}, \code{PN}, \code{NP}, \code{NN}};
#'   \item{\code{Ch1.Ch2.}, \code{Ch1.Ch2..1}, \code{Ch1.Ch2..2},
#'   \code{Ch1.Ch2..3}};
#'   \item{\code{Ch1+Ch2+}, \code{Ch1+Ch2-}, \code{Ch1-Ch2+}, \code{Ch1-Ch2-}};
#'   or
#'   \item{\code{Ch1pCh2p}, \code{Ch1pCh2n}, \code{Ch1nCh2p}, \code{Ch1nCh2n}}.
#' }
#' @param rows A vector of rows (numbers or well names) to keep from the
#' original data frame. If set to \code{NULL}, all wells will be used. Defaults
#' to \code{NULL}.
#' @param rowID If set, this field is used as the row names. If \code{NULL},
#' the existing row names from \code{df} are used. Defaults to \code{NULL}.
#' @param keepCols A vector of columns to keep from \code{df}. If \code{NULL},
#' no extra columns are added. Defaults to \code{NULL}.
#' @param keepColNames A vector of new column names for \code{keepCols}. If
#' \code{NULL}, the column names from \code{keepCols} are reused. Defaults to
#' \code{NULL}.
#'
#' @return A data frame with the counts in the \code{PP}, \code{PN}, \code{PN}
#' and \code{PN} convention.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Take a data frame with row names given by the well names. Get a simple
#' ## count of droplets in each cluster.
#' parseClusterCounts(KRAScounts)
#'
#' ## Keep only the row numbers 3, 6 and 9.
#' parseClusterCounts(KRAScounts, rows=c(3, 6, 9))
#'
#' ## Keep only the rows labelled "F03", "A04", "D04".
#' parseClusterCounts(KRAScounts, rows=c("F03", "A04", "D04"))
#'
#' ## Take a data frame with a 'Well' column and do the same as above.
#' parseClusterCounts(KRAScountsWellCol, rowID="Well")
#'
#' ## Keep the 'InputAmount' column.
#' parseClusterCounts(KRAScounts, keepCols=c("InputAmount"))
#'
#' ## Keep the 'InputAmount' column and rename it.
#' parseClusterCounts(KRAScounts, keepCols=c("InputAmount"),
#'                    keepColNames=c("NanogramsIn"))
#'
#' @export

parseClusterCounts <- function(df, rows=NULL, rowID=NULL,
                               keepCols=NULL, keepColNames=NULL) {
  numWells <- .numberOfWells(df)

  # The number of rows to use is given by the number of unique wells.
  if(is.null(rows)) {
    rows <- seq_len(numWells)
  } else {
    # If characters, they will be unique.
    if(is.character(rows)) {
      # Find the indices of the matches.
      rows <- match(unique(rows), .getWellNames(df))
      if(any(is.na(rows))) {
        stop("Not all well names specified in 'rows' are present in 'df'.")
      }
    } else {
      # Numbered rows given: check that there are no repeats.
      if("Well" %in% colnames(df)) {
        selectedWellNames <- df[rows, "Well"]
      } else if(is.character(rownames(df))) {
        selectedWellNames <- rownames(df[rows, ])
      }

      rows <- match(unique(selectedWellNames), .getWellNames(df))
    }
  }

  # Set the rownames as the wells.
  if("Well" %in% colnames(df)) {
    rownames(df)[seq(numWells)] <- as.character(df$Well[seq(numWells)])
  }

  # Find the droplet counts in the different channels. We deal with a variety
  # of column name styles.
  if(all(c("Ch1.Ch2.", "Ch1.Ch2..1", "Ch1.Ch2..2", "Ch1.Ch2..3") %in%
         colnames(df))) {
    pp <- "Ch1.Ch2."
    pn <- "Ch1.Ch2..1"
    np <- "Ch1.Ch2..2"
    nn <- "Ch1.Ch2..3"
  } else if(all(c("Ch1+Ch2+", "Ch1+Ch2-", "Ch1-Ch2+", "Ch1-Ch2-") %in%
                colnames(df))) {
    pp <- "Ch1+Ch2+"
    pn <- "Ch1+Ch2-"
    np <- "Ch1-Ch2+"
    nn <- "Ch1-Ch2-"
  } else if(all(c("Ch1pCh2p", "Ch1pCh2n", "Ch1nCh2p", "Ch1nCh2n") %in%
                colnames(df))) {
    pp <- "Ch1pCh2p"
    pn <- "Ch1pCh2n"
    np <- "Ch1nCh2p"
    nn <- "Ch1nCh2n"
  } else if(all(c("PP", "PN", "NP", "NN") %in% colnames(df))) {
    pp <- "PP"
    pn <- "PN"
    np <- "NP"
    nn <- "NN"
  } else {
    stop("Droplet channel counts not found. See ?parseClusterCounts for ",
         "accepted\ncolumn name formats.")
  }

  # Some of the given rows are not in df.
  if(any(!rows %in% seq_len(nrow(df)))) {
    stop("Argument 'rows' contains indices not in 'df'.")
  }

  # Create data frame with basic columns.
  d <- data.frame("PP"=df[rows, pp], "PN"=df[rows, pn],
                  "NP"=df[rows, np], "NN"=df[rows, nn])

  # Add row names if needed.
  if(nrow(df) > 1) {
    if(is.null(rowID)) {
      rn <- rownames(df)[rows]
    } else {
      rn <- df[rows, rowID]
    }
    d <- data.frame(rn, d, row.names=1)
  }

  # Columns specified---retain these only.
  if(!is.null(keepCols)) {
    # Don't rename the columns to keep.
    if(is.null(keepColNames)) {
      keepColNames <- keepCols
    }

    d[, keepColNames] <- df[rows, keepCols]
  }

  d
}


#' Get a vector of essential dependent columns.
#'
#' Get a list of the essential columns in the ddPCR data that depend on the
#' target (e.g. mutant or wild type).
#'
#' @param prefix A string to prepend to each of the essential column names.
#' Defaults to \code{""} (the empty string).
#'
#' @return A vector of essential column names with an optional prefix.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.essentialDependentCols <- function(prefix="") {
  dc <- c("Positives",
          "Negatives",
          "Concentration",
          "CopiesPer20uLWell",
          "PoissonConfMax",
          "PoissonConfMin",
          "PoissonConfMax68",
          "PoissonConfMin68")
  paste0(prefix, dc)
}


#' Get a vector of all dependent columns.
#'
#' Get a list of the columns in the ddPCR data that depend on the target (e.g.
#' mutant or wild type). This includes columns that cannot be calculated from
#' the very basic droplet counts.
#'
#' @param prefix A string to prepend to each of the column names. Defaults to
#' \code{""} (the empty string).
#'
#' @return A vector of column names with an optional prefix.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.dependentCols <- function(prefix="") {
  dc <- c(.essentialDependentCols(),
          "Threshold",
          "MeanAmplitudeofPositives",
          "MeanAmplitudeofNegatives",
          "MeanAmplitudeTotal")
  paste0(prefix, dc)
}


#' Makes a long form data frame into wide form.
#'
#' Returns a data frame with the dependent columns prefixed with given labels
#' (depending on the targets). All relevant columns are retained.
#'
#' @param df A data frame created by calling \code{read.csv} on the raw ddPCR
#' output.
#' @param ch1Label The prefix to use for the channel 1 target. Defaults to
#' "Mt".
#' @param ch2Label The prefix to use for the channel 2 target. Defaults to
#' "Wt".
#' @param rows The number of rows to retain from the original data frame. If
#' \code{NULL}, all of the wells are used. Defaults to \code{NULL}.
#'
#' @return A data frame with the target rows merged.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Cast output from Bio-Rad's QuantaSoft into wide form.
#' castSummary(KRAScountsQS)
#'
#' ## Only retain selected rows.
#' castSummary(KRAScountsQS, rows=c(1,4:6))
#'
#' @export

castSummary <- function(df, ch1Label="Mt", ch2Label="Wt", rows=NULL) {
  # Stop if the labels are the same.
  if(ch1Label == ch2Label) {
    stop("The labels 'ch1Label' and 'ch2Label' should be different.")
  }

  # We only need to look at the top half for the ch1 data data.
  if(is.null(rows)) {
    rows <- seq_len(nrow(df)/2)
  }

  # Create a fresh data frame with the independent columns.
  d <- parseClusterCounts(
    df,
    rows,
    rowID="Well",
    keepCols=c(
      "AcceptedDroplets",
      "Ratio",
      "PoissonRatioMax",
      "PoissonRatioMin",
      "PoissonRatioMax68",
      "PoissonRatioMin68",
      "FractionalAbundance",
      "PoissonFractionalAbundanceMax",
      "PoissonFractionalAbundanceMin",
      "PoissonFractionalAbundanceMax68",
      "PoissonFractionalAbundanceMin68",
      "Linkage"
    ),
    keepColNames=c(
      "AcceptedDroplets",
      "Ratio",
      "PoisRatioMax",
      "PoisRatioMin",
      "PoisRatioMax68",
      "PoisRatioMin68",
      "FracAbun",
      "PoisFracAbunMax",
      "PoisFracAbunMin",
      "PoisFracAbunMax68",
      "PoisFracAbunMin68",
      "Linkage"
    )
  )
  # Prepend the labels to the dependent column names.
  d[, .dependentCols(ch1Label)] <- df[rows, .dependentCols()]
  d[, .dependentCols(ch2Label)] <- df[rows+(nrow(df)/2), .dependentCols()]

  d
}


#' Checks a data frame is a wide-form table.
#'
#' Our preferred data frame format is to have things in a wide-form data frame,
#' i.e. to have channel 1 and channel 2 data both in the same row.
#'
#' @param df A data frame.
#' @param ch1Label The prefix to use for the channel 1 target. Defaults to
#' "Mt".
#' @param ch2Label The prefix to use for the channel 2 target. Defaults to
#' "Wt".
#'
#' @return \code{TRUE} if \code{df} is considered to be of the correct format
#' and \code{FALSE} otherwise.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}


.isWideForm <- function(df, ch1Label="Mt", ch2Label="Wt") {
  any(c(.essentialDependentCols(ch1Label),
        .essentialDependentCols(ch2Label)) %in% colnames(df))
}


#' Take a data frame and compute the abundance of molecules.
#'
#' Returns a data frame with basic counts, the concentration of each kind of
#' target molecule, the ratio ch1:ch2 molecules, and the fractional abundance
#' of ch1 molecules in the overall count.
#'
#' @param df A data frame with droplet count columns in one of the following
#' formats:
#' \itemize{
#'   \item{\code{PP}, \code{PN}, \code{NP}, \code{NN}};
#'   \item{\code{Ch1.Ch2.}, \code{Ch1.Ch2..1}, \code{Ch1.Ch2..2},
#'   \code{Ch1.Ch2..3}};
#'   \item{\code{Ch1+Ch2+}, \code{Ch1+Ch2-}, \code{Ch1-Ch2+}, \code{Ch1-Ch2-}};
#'   or
#'   \item{\code{Ch1pCh2p}, \code{Ch1pCh2n}, \code{Ch1nCh2p}, \code{Ch1nCh2n}}.
#' }
#' @param ch1Label The prefix to use for the channel 1 target. Defaults to
#' "Mt".
#' @param ch2Label The prefix to use for the channel 2 target. Defaults to
#' "Wt".
#' @param rows A vector of rows (numbers or well names) to keep from the
#' original data frame. If set to \code{NULL}, all wells will be used. Defaults
#' to \code{NULL}.
#' @param rowID If set, this field is used as the row names. If \code{NULL},
#' the existing row names from \code{df} are used. Defaults to \code{NULL}.
#' @param keepCols A vector of columns to keep from \code{df}. If \code{NULL},
#' no extra columns are added. Defaults to \code{NULL}.
#' @param keepColNames A vector of new column names for \code{keepCols}. If
#' \code{NULL}, the column names from \code{keepCols} are reused. Defaults to
#' \code{NULL}.
#'
#' @return A data frame with
#' \itemize{
#'   \item \code{rowID} as the row names (if given);
#'   \item the droplet counts per channel;
#'   \item the number of ch1 and ch2 positive and negative readings;
#'   \item the ch1 and ch2 concentration, copies per 20ul, and total copies per
#'   20ul;
#'   \item the ratio of ch1 to ch2 molecules; and
#'   \item the fractional abundance of ch1 molecules in the overall molecule
#'   count (as a percentage).
#' }
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Take a data frame with row names given by the well names. Get estimates
#' ## for the numbers of molecules in each sample..
#' fullCountsSummary(KRAScounts)
#'
#' ## Keep only the row numbers 3, 6 and 9.
#' fullCountsSummary(KRAScounts, rows=c(3, 6, 9))
#'
#' ## Keep only the rows labelled "F03", "A04", "D04".
#' fullCountsSummary(KRAScounts, rows=c("F03", "A04", "D04"))
#'
#' ## Take a data frame with a 'Well' column and do the same as above.
#' fullCountsSummary(KRAScountsWellCol, rowID="Well")
#'
#' ## Keep the 'InputAmount' column.
#' fullCountsSummary(KRAScounts, keepCols=c("InputAmount"))
#'
#' ## Keep the 'InputAmount' column and rename it.
#' fullCountsSummary(KRAScounts, keepCols=c("InputAmount"),
#'                   keepColNames=c("NanogramsIn"))
#' @export

fullCountsSummary <- function(df, ch1Label="Mt", ch2Label="Wt",
                              rows=NULL, rowID=NULL, keepCols=NULL,
                              keepColNames=NULL) {
  # Basic numbers.
  d <- parseClusterCounts(df, rows, rowID, keepCols, keepColNames)
  d$AcceptedDroplets <- d$PP + d$PN + d$NP + d$NN
  d[, paste0(ch1Label, "Positives")] <- d$PP + d$PN
  d[, paste0(ch1Label, "Negatives")] <- d$NP + d$NN
  d[, paste0(ch2Label, "Positives")] <- d$PP + d$NP
  d[, paste0(ch2Label, "Negatives")] <- d$PN + d$NN

  # Concentration.
  d[, paste0(ch1Label, "Concentration")] <-
    -log(1 - d[, paste0(ch1Label, "Positives")] / d$AcceptedDroplets) /
      ddpcr$dropletVolume
  d[, paste0(ch2Label, "Concentration")] <-
    -log(1 - d[, paste0(ch2Label, "Positives")] / d$AcceptedDroplets) /
      ddpcr$dropletVolume
  d[, paste0(ch1Label, "CopiesPer20uLWell")] <-
    20 * d[, paste0(ch1Label, "Concentration")]
  d[, paste0(ch2Label, "CopiesPer20uLWell")] <-
    20 * d[, paste0(ch2Label, "Concentration")]
  d$TotalCopiesPer20uLWell <-
    d[, paste0(ch1Label, "CopiesPer20uLWell")] +
    d[, paste0(ch2Label, "CopiesPer20uLWell")]

  # Ratio.
  d$Ratio <- log(1 - d[, paste0(ch1Label, "Positives")] / d$AcceptedDroplets) /
    log(1 - d[, paste0(ch2Label, "Positives")] / d$AcceptedDroplets)

  # Fractional abundance.
  d$FracAbun <-
    100 * log(1 - d[, paste0(ch1Label, "Positives")] / d$AcceptedDroplets) /
    (log(1 - d[, paste0(ch1Label, "Positives")] / d$AcceptedDroplets) +
     log(1 - d[, paste0(ch2Label, "Positives")] / d$AcceptedDroplets))

  d
}
