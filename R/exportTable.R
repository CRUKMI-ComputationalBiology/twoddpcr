#' @import methods
#' @import utils
NULL

#' @title Exports an object to file.
#'
#' @param theObject The dataframe to export.
#' @param location The location to export to. This should be a filename if we
#' are using \code{exportZip}, or we are using \code{exportTable} and
#' \code{theObject} is a data frame or \code{ddpcrWell} object. If
#' \code{theObject} is a \code{ddpcrPlate} object, this should be
#' a directory.
#' @param delim The character to use as a field separator. Defaults to ",",
#' i.e. export a CSV.
#' @param ... Other options depending on the type of \code{theObject}.
#'
#' @details Note that filenames of the form \code{Anything_A01_Amplitude.csv}
#' can be read by \code{\link{readCSVDataFrame}} so that the well name can be
#' extracted successfully (in this case \code{A01}). Where it is used, see the
#' default value of the parameter \code{suffix}.
#'
#' @return Exports a file.
#'
#' @name exportTable
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Output to a temporary directory.
#' tmpOut <- file.path(normalizePath(tempdir()))
#'
#' ## Read some counts data and generate a summary data frame.
#' df <- fullCountsSummary(KRAScountsQS)
#' summaryDf <- fullCopiesSummary(df)
#'
#' ## Write the summary to a CSV file.
#' exportTable(summaryDf, file.path(tmpOut, "summary-table.csv"))
#'
#' ## Write the summary to a tab-separated text file.
#' exportTable(summaryDf, file.path(tmpOut, "summary-table.txt"), delim="\t")
#'
#' ## Write the summary to a CSV file with leading column labelled "Patient".
#' exportTable(summaryDf, file.path(tmpOut, "summary-table.csv"),
#'             leadingColName="Patient")
#'
#' ## Read a droplet amplitude CSV file to a ddpcrWell object.
#' ampFile <- system.file("extdata", "amplitudes", "sample_B03_Amplitude.csv",
#'                        package="twoddpcr")
#' aWell <- ddpcrWell(well=ampFile)
#'
#' ## Classify the droplets into 4 clusters.
#' aWell <- kmeansClassify(aWell, centres=4)
#'
#' ## Write the amplitudes to a CSV file with the old and new classifications.
#' exportTable(aWell,
#'   location=file.path(tmpOut, "With_Kmeans_B03_Amplitude.csv"))
#'
#' ## Write the amplitudes to a CSV file with the new classification only.
#' exportTable(aWell,
#'   location=file.path(tmpOut, "With_Kmeans_B03_Amplitude.csv"),
#'   cMethod="kmeans")
#'
#' ## Read all amplitude files in a directory to a ddpcrPlate object.
#' moreAmpsDir <- system.file("extdata", "more-amplitudes", package="twoddpcr")
#' krasPlate <- ddpcrPlate(wells=moreAmpsDir)
#'
#' ## Classify the droplets into 4 clusters.
#' krasPlate <- kmeansClassify(krasPlate, centres=4)
#'
#' ## Write the amplitudes to multiple files in a directory with the old and
#' ## new classifications.
#' exportTable(krasPlate, location=file.path(tmpOut, "amplitudes-classified"))
#'
#' ## Write the amplitudes to multiple files with the new classification only
#' ## and a custom prefix for the filenames.
#' exportTable(krasPlate, location=file.path(tmpOut, "amplitudes-classified"),
#'             cMethod="kmeans", prefix="Kmeans_Only_")
#'
#' ## Export to a zip file.
#' exportZip(krasPlate,
#'           location=file.path(tmpOut, "amplitudes-classified/all.zip"),
#'           cMethod="kmeans", prefix="Kmeans_Only_")
#'
#'
#'
#' @export

setGeneric("exportTable", function(theObject, location, delim=",", ...) {
  standardGeneric("exportTable")
})

#' @rdname exportTable
#'
#' @description If given a data frame, \code{exportTable} exports the whole
#' data frame to file. This could be a data frame of any form. A few options
#' are available that can be used to determine the format of the file that is
#' exported, e.g. using a heading for the row names 'column', or omitting row
#' names altogether.
#'
#' @param leadingColName The name of the leading column, i.e. the 'row names'
#' of the dataframe. This could be a patient identifier or the well used in the
#' ddPCR experiment. If \code{NULL}, the exported heading will be an empty
#' string. Defaults to \code{NULL}.
#' @param row.names If \code{NULL}, exports a column corresponding to the row
#' names; if \code{FALSE}, no such column is included. If 'leadingColName' is
#' not \code{FALSE}, row.names is assumed to be \code{FALSE}. Defaults to
#' \code{TRUE}.
#'
#' @exportMethod exportTable

setMethod(
  "exportTable", "data.frame",
  function(theObject, location, delim=",",
           leadingColName=NULL, row.names=TRUE) {
    if(is.null(leadingColName)) {
      if(delim == ",") {
        write.csv(theObject, location, row.names=row.names)
      } else {
        write.table(theObject, location, sep=delim, col.names=NA,
                    row.names=row.names)
      }
    } else {
      # Insert the row names as the leading column and name it.
      d <- data.frame(rownames(theObject), theObject, check.names=FALSE)
      colnames(d)[1] <- leadingColName

      if(delim == ",") {
        write.csv(d, location, row.names=FALSE)
      } else {
        write.table(d, location, sep=delim, row.names=FALSE)
      }
    }
  }
)


#' @rdname exportTable
#'
#' @description If a \code{ddpcrWell} is given, \code{exportTable} exports to
#' a single file with specified/all classification methods.
#'
#' @param cMethod The name or column number of the classification methods in
#' a \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}} object to export
#' to file. If \code{NULL}, all of the classification methods are exported.
#' Defaults to \code{NULL}.
#'
#' @exportMethod exportTable

setMethod(
  "exportTable", "ddpcrWell",
  function(theObject, location, delim=",", cMethod=NULL) {
    df <- wellClassification(theObject, cMethod=cMethod, withAmplitudes=TRUE)
    exportTable(theObject=df, location=location, delim=delim,
                leadingColName=NULL, row.names=FALSE)
  }
)


#' @rdname exportTable
#'
#' @description If a \code{ddpcrPlate} is given, \code{exportTable}
#' exports to a directory in the given \code{location}, where one file is
#' created for each of the wells. If it does not exist, the directory
#' \code{location} will be created as long as all other parent directories
#' exist.
#'
#' @param prefix For \code{ddpcrPlate} objects, this is the prefix to
#' prepend to the output filenames.
#' @param suffix For \code{ddpcrPlate} objects, this is the suffix to
#' append to the output filenames. This is typically the filename extension,
#' e.g. ".csv" or ".txt". Defaults to ".csv".
#'
#' @exportMethod exportTable

setMethod(
  "exportTable", "ddpcrPlate",
  function(theObject, location, delim=",", cMethod=NULL, prefix="",
           suffix="_Amplitude.csv") {
    if(!dir.exists(location)) {
      dir.create(file.path(location))
    }

    # Get the data frame to export.
    df <- plateClassification(theObject, cMethod=cMethod, withAmplitudes=TRUE)

    # Export individual files.
    fNames <- vapply(
      names(theObject),
      function(w) {
        filePath <- paste0(location, "/", prefix, w, suffix)
        exportTable(theObject=df[[w]], location=filePath,
                    delim=delim, leadingColName=NULL,
                    row.names=FALSE)
        filePath
      },
      character(1)
    )
    invisible(fNames)
  }
)


#' @description \code{exportZip} takes a \code{ddpcrPlate} object and
#' exports it as a zip file.
#'
#' @rdname exportTable
#'
#' @export

setGeneric("exportZip",
  function(theObject, location, delim=",", cMethod=NULL, prefix="",
           suffix="_Amplitude.csv") {
    standardGeneric("exportZip")
  }
)

#' @rdname exportTable
#'
#' @exportMethod exportZip

setMethod("exportZip", "ddpcrPlate",
  function(theObject, location, delim=",", cMethod=NULL, prefix="",
           suffix="_Amplitude.csv") {
    # Remember where we are.

    # Get the absolute path of the file to export to.
    locDir <- normalizePath(dirname(location))
    locBase <- basename(location)
    location <- paste(locDir, locBase, sep="/")

    tryCatch({
      # Make a temporary directory and go there.
      tmpDir <- normalizePath(tempdir())
      if(!dir.exists(tmpDir))
        dir.create(file.path(tmpDir))
      owd <- setwd(tmpDir)
      on.exit(setwd(owd))  # return to the original working directory on exit

      # Create the zip file.
      outDirName <- dirname(location)
      if(!dir.exists(outDirName)) {
        dir.create(outDirName)
      }
      zipFile <- zip(zipfile=location,
                     files=exportTable(theObject=theObject, location=".",
                                       delim=delim, cMethod=cMethod,
                                       prefix=prefix, suffix=suffix))
      if(file.exists(paste0(location, ".zip"))) {
        file.rename(paste0(location, ".zip"), location)
      }
    },
    error=function(e) {
      stop(e)
    })
  }
)
