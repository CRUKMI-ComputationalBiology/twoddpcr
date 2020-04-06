#' Try to get well names from a vector of filenames.
#'
#' If each of the given filenames are of the form
#' "<PlateName>_<WellName>_Amplitude.csv", where <WellName> is of the form A01,
#' B01, etc., then this function can extract the <WellName> component.
#' Otherwise, the whole file name is assumed to be the well name.
#'
#' @param filenames A character vector of filenames with .csv extension.
#'
#' @return A character vector of well names.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get the well names (recognised format).
#' extractWellNames(c("Sample_Plate_Name_G02_Amplitude.csv",
#'                    "Sample_Plate_Name_H02_Amplitude.csv",
#'                    "Sample_Plate_Name_A03_Amplitude.csv",
#'                    "Sample_Plate_Name_B03_Amplitude.csv"))
#'
#' ## Get the well names (unrecognised format).
#' extractWellNames(c("Sample_G02.csv",
#'                    "Sample_H02.csv",
#'                    "Sample_A03.csv",
#'                    "Sample_B03.csv"))
#'
#' @export

extractWellNames <- function(filenames) {
  if(all(grepl("_Amplitude.csv$", filenames))) {
    vapply(gsub("_Amplitude\\.csv$", "", filenames),
           function(f) { substr(f, nchar(f)-2, nchar(f)) }, character(1))
  } else {
    # Assume that the file name is the well name.
    gsub("\\.csv$", "", basename(filenames))
  }
}


#' Try to get plate name from a filename.
#'
#' If the given filename is of the form "<PlateName>_<WellName>_Amplitude.csv",
#' where <WellName> is of the form A01, B01, etc., then this function can
#' extract the <PlateName> component. Otherwise, an empty string is returned.
#'
#' @param filename A character string corresponding to a filename with .csv
#' extension.
#'
#' @return A character string corresponding to the plate name. This is "" if
#' filename is not in a known format.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get the plate name (recognised format).
#' extractPlateName(c("Sample_Plate_Name_G02_Amplitude.csv"))
#'
#' ## Get the plate name (unrecognised format).
#' extractPlateName(c("Sample_G02.csv"))
#'
#' @export

extractPlateName <- function(filename) {
  if(all(grepl("_Amplitude.csv$", filename))) {
    vapply(gsub("_Amplitude\\.csv$", "", filename),
           function(f) { substr(f, 0, nchar(f)-4) }, character(1))
  } else {
    # Unknown format.
    ""
  }
}


#' Read all given CSV files into a list.
#'
#' Bio-Rad's QuantaSoft can export droplet amplitude data from multiple wells
#' into CSV files for each well. This function can read these CSV files into
#' a list. Note that empty wells will be ignored.
#'
#' @param path The path containing the CSV files (can be a combination of
#' directories and individual CSV file paths). Each file will have
#' a \code{Ch1.Amplitude}, \code{Ch2.Amplitude} and possibly classification
#' columns, e.g. by default, QuantaSoft returns a \code{Cluster} column too.
#' @param wellCol If \code{TRUE}, an additional column is added with the well
#' name.  This is useful if we need to merge all the data in the output list
#' and we want to identify the original well of each droplet. Defaults to
#' \code{FALSE}.
#' @param sortByLetter If \code{TRUE}, the resulting list is sorted by the
#' letter in the well names first, e.g. "A02" comes before "B01". If
#' \code{FALSE}, the result is sorted by the numeric component of the well
#' names first, e.g. "B01" comes before "A02". Defaults to \code{FALSE}.
#'
#' @return A list of data frames, each containing the data from a CSV file with
#' the corresponding well name.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Read all of the droplet amplitudes from CSV files in a directory.
#' moreAmpsDir <- system.file("extdata", "more-amplitudes", package="twoddpcr")
#' someWells <- readCSVDataFrame(moreAmpsDir)
#'
#' ## We can read files from directories and files at the same time.
#' ampFile <- system.file("extdata", "amplitudes", "sample_B03_Amplitude.csv",
#'                        package="twoddpcr")
#' someWells <- readCSVDataFrame(c(moreAmpsDir, ampFile))
#'
#' ## If samples have been ordered by "A01", "A02", "A03", etc. instead of
#' ## "A01", "B01", "C01", etc., we can set the sortByLetter flag to TRUE.
#' someWells <- readCSVDataFrame(moreAmpsDir, sortByLetter=TRUE)
#'
#' ## Setting wellCol to TRUE adds an extra column with the well name. If we
#' ## bind the data frames together, we can track where the droplets came from.
#' someWells <- readCSVDataFrame(moreAmpsDir, wellCol=TRUE)
#' someWells <- do.call(rbind, someWells)
#' head(someWells)
#' tail(someWells)
#'
#' @export

readCSVDataFrame <- function(path, wellCol=FALSE, sortByLetter=FALSE) {
  if(!all(file.exists(path))) {
    stop("Not all files and/or directories exist.")
  }

  # Directories.
  dirFiles <- list.files(path=path[dir.exists(path)], pattern="*.csv$",
                         full.names=TRUE)
  # Files.
  filenames <- c(dirFiles, path[!dir.exists(path)])

  # If possible, extract the well name from the filenames.
  wellNames <- extractWellNames(filenames)
  names(filenames) <- wellNames

  # Read the files.
  tryCatch({
    wellData <- lapply(filenames, utils::read.csv)

    # Remove any empty files.
    nonemptyWells <- (vapply(wellData, nrow, numeric(1)) != 0)
    names(nonemptyWells) <- NULL
    wellData <- wellData[nonemptyWells]
    wellNames <- wellNames[nonemptyWells]
  },
  error=function(e) {
    stop("Error in read.csv: Perhaps the header row has a differing number ",
         "of columns to\nthe rest of the file? ",
         "(Check rows for trailing commas.)")
  })

  if(wellCol) {
    wellData <- lapply(seq_along(wellData),
                       function(i) {
                         if(nrow(wellData[[i]]) > 0) {
                           data.frame(wellData[[i]], "Well"=wellNames[[i]])
                         } else {
                           wellData[[i]]
                         }
                       })
  }
  names(wellData) <- wellNames

  # How to sort the wells?
  if(sortByLetter) {
    wellData[order(names(wellData))]
  } else {
    wellData[order(substr(names(wellData), start=2, stop=3),
                   substr(names(wellData), start=1, stop=1))]
  }
}

