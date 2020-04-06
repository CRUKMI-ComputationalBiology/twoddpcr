#' @include global.R
#' @import methods
#' @import utils
NULL

#' An S4 class for the classification of a single well in a ddPCR
#' experiment.
#'
#' @slot dropletAmplitudes A data frame with columns \code{Ch1.Amplitude}
#' and \code{Ch2.Amplitude} corresponding to all the droplets in the ddPCR
#' well.
#' @slot classification A vector of factors, where the levels are given by
#' \code{ddpcr$classesRain}.
#'
#' @name ddpcrWell-class
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @export ddpcrWell
#' @exportClass ddpcrWell

.ddpcrWell <-
  setClass("ddpcrWell",
    slots=c(
      dropletAmplitudes="data.frame",
      classification="data.frame"
    ),
    prototype=list(
      dropletAmplitudes=data.frame("Ch1.Amplitude"=double(),
                                   "Ch2.Amplitude"=double()),
      classification=data.frame(factor(c(), levels=ddpcr$classesRain))
    ),
    validity=function(object) {
      if(!all(c("Ch1.Amplitude", "Ch2.Amplitude") %in%
              colnames(object@dropletAmplitudes))) {
        return(paste("The droplet amplitudes data frame does not have the ",
                     "correct column names."))
      }
      if(nrow(object@classification) > 0 &&
              nrow(object@dropletAmplitudes) != nrow(object@classification)) {
        return(paste("The number of droplets is not the same as the number ",
                     "of classifications."))
      }
      return(TRUE)
    }
  )


#' Extract a classification from a data frame.
#'
#' Check that non-Ch*Amplitude columns are in a known classification format and
#' coerce it to a factor.
#'
#' @param colName The name of the column to focus on.
#' @param well The data frame from which to extract the classifications.
#'
#' @return A factor with levels in \code{ddpcr$classesRain}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.getClassificationData <- function(colName, well) {
  if(!colName %in% c("Ch1.Amplitude", "Ch2.Amplitude")) {
    # "NN", "NP", "PN", "PP" format.
    if((is.factor(well[, colName])
        && all(levels(well[, colName]) %in% ddpcr$classesRain))
       || (is.character(well[, colName])
           && all(well[, colName] %in% ddpcr$classesRain))) {
      factor(well[, colName], levels=ddpcr$classesRain)
    } else if(all(unique(well[, colName]) %in% c(1, 2, 3, 4))) {
      # QuantaSoft format.
      factor(ifelse(well[, colName]==1, ddpcr$nn,
             ifelse(well[, colName]==2, ddpcr$pn,
             ifelse(well[, colName]==4, ddpcr$np,
                                        ddpcr$pp))),
             levels=ddpcr$classesRain)
    } else {
      warning("Ignoring column ", colName, " (unrecognised format).")
      data.frame(row.names=seq_len(nrow(well)))
    }
  } else {
    data.frame(row.names=seq_len(nrow(well)))
  }
}


#' The constructor for the \code{ddpcrWell} class.
#'
#' @param well A well with columns \code{Ch1.Amplitude} and
#' \code{Ch2.Amplitude} and optional classification columns. This can be in the
#' form of a data frame or the path to a droplet amplitude CSV file.
#'
#' @return A \code{ddpcrWell} object with the given droplets in the well.
#'
#' @rdname ddpcrWell-class
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## A \code{ddpcrWell} object can be created from data from a list of data
#' ## frames.
#' ddpcrWell(KRASdata[[1]])
#'
#' ## An CSV file of droplet amplitudes can also be loaded.
#' ampFile <- system.file("extdata/amplitudes/sample_B03_Amplitude.csv",
#'                        package="twoddpcr")
#' ddpcrWell(ampFile)
#'
#' @export

setGeneric("ddpcrWell", function(well) {
  standardGeneric("ddpcrWell")
})

#' @rdname ddpcrWell-class
#'
#' @export

setMethod("ddpcrWell", "data.frame", function(well) {
    # Make the data frame in the right format.
    well <- setChannelNames(well)

    # Try to make a ddpcrWell object the data frame.
    if(all(c("Ch1.Amplitude", "Ch2.Amplitude") %in% colnames(well))) {
      # The droplet amplitudes.
      amps <- well[, c("Ch1.Amplitude", "Ch2.Amplitude")]

      # If they exist, get the classification(s).
      # print(colnames(well))
      cl <- lapply(colnames(well), .getClassificationData, well)
      names(cl) <- colnames(well)
      cl <- as.data.frame(do.call(cbind, cl))

      # Add a default "None" column to droplet classifications (if needed).
      dc <- factor(rep(ddpcr$na, nrow(well)), levels=ddpcr$classesRain)
      if(!is.null(cl) && !"None" %in% colnames(cl)) {
        cl <- data.frame("None"=dc, cl)
      } else if(is.null(cl)) {
        cl <- data.frame("None"=dc)
      }

      # Set the classifications.
      .ddpcrWell(dropletAmplitudes=amps, classification=cl)
    } else {
      stop("The given 'well' is missing the 'Ch1.Amplitude' and ",
           "'Ch2.Amplitude' columns.")
    }
  }
)


#' @rdname ddpcrWell-class
#'
#' @export

setMethod("ddpcrWell", "character", function(well) {
  # Character string well: hopefully a filename.
  if(is.character(well)) {
    # Just look at the first one.
    well <- well[1]

    # File or directory path.
    if(file.exists(well)) {
      well <- readCSVDataFrame(well)[[1]]
      if(nrow(well) == 0)
        warning("No droplet amplitudes loaded. Was this intended?")
      ddpcrWell(well)
    } else {
      stop("The string 'well' is not a file.")
    }
  }
})


#' @rdname ddpcrWell-class
#'
#' @export

setMethod("ddpcrWell", "missing", function(well) {
  well <- data.frame("Ch1.Amplitude"=double(), "Ch2.Amplitude"=double())
  ddpcrWell(well)
})


#' @rdname ddpcrWell-class
#'
#' @export

setMethod("ddpcrWell", "ddpcrWell", function(well) {
  well
})


#' @title Retrieve droplet amplitudes.
#'
#' @description Retrieve the droplet amplitudes from an object.
#'
#' @param theObject A \code{\link{ddpcrWell}} or \code{\link{ddpcrPlate}}
#' object.
#'
#' @return If \code{theObject} is a \code{\link{ddpcrWell}} object, return
#' a data frame of droplet amplitudes with columns "Ch1.Amplitude" and
#' "Ch2.Amplitude". If \code{theObject} is a \code{\link{ddpcrPlate}} object,
#' return a list of data frames.
#'
#' @seealso \code{\link{wellClassification}} for the classification of the
#' droplets.
#'
#' @name amplitudes
#'
#' @examples
#' ## Set a ddpcrWell object with no data.
#' aWell <- ddpcrWell(well=data.frame("Ch1.Amplitude"=double(),
#'                                    "Ch2.Amplitude"=double()))
#'
#' ## This can be checked to be empty.
#' amplitudes(aWell)
#'
#' ## Alternatively, load some data.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#'
#' ## We check again and see that it has been populated.
#' head(amplitudes(aWell))
#'
#' # Get all of the KRASdata droplet amplitudes.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' allDroplets <- amplitudes(krasPlate)
#' str(allDroplets)
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @export

setGeneric("amplitudes", function(theObject) {
  standardGeneric("amplitudes")
})

#' @rdname amplitudes
#'
#' @exportMethod amplitudes

setMethod("amplitudes", "ddpcrWell", function(theObject) {
  theObject@dropletAmplitudes
})


#' Retrieve a classification vector.
#'
#' Retrieve the classification from a \code{\link{ddpcrWell}} object.
#'
#' @param theObject A \code{\link{ddpcrWell}} object.
#' @param cMethod The names (or column numbers) of the classification to
#' retrieve. If \code{NULL}, then all of the classifications are obtained.
#' Defaults to \code{NULL} when retrieving. When setting a classification, this
#' cannot be \code{NULL}.
#' @param withAmplitudes Logical value. If \code{TRUE}, returns a data frame
#' with the droplet amplitudes and corresponding classifications. If
#' \code{FALSE}, returns the classification vector only. Defaults to
#' \code{FALSE}.
#' @param value A factor with the same length as the number of droplets in
#' \code{theObject}, with levels in \code{ddpcr$classesRain}.
#'
#' @return A factor or data frame corresponding to the requested
#' classification(s).
#'
#' @seealso \code{\link{wellClassificationMethod}} for the name of the
#' classification method.
#'
#' @examples
#' ## Take some droplets with a given classification.
#' amplitudes <- KRASdata[["E03"]][, c("Ch1.Amplitude", "Ch2.Amplitude")]
#'
#' ## Create a ddpcrWell object with the amplitudes only.
#' aWell <- ddpcrWell(well=amplitudes)
#'
#' ## This has no classification yet.
#' head(wellClassification(aWell))
#'
#' ## We check the classification now, showing the amplitudes as well.
#' head(wellClassification(aWell, withAmplitudes=TRUE))
#'
#' ## Now set a sample classification.
#' wellClassification(aWell, cMethod="Sample") <-
#'   rep(c("NN", "NP", "PN", "PP"), numDroplets(aWell) / 4)
#' head(wellClassification(aWell, withAmplitudes=TRUE))
#'
#' @name wellClassification
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @export

setGeneric("wellClassification",
  function(theObject, cMethod=NULL, withAmplitudes=FALSE) {
    standardGeneric("wellClassification")
  }
)

#' @rdname wellClassification
#'
#' @exportMethod wellClassification

setMethod("wellClassification", "ddpcrWell",
  function(theObject, cMethod=NULL, withAmplitudes=FALSE) {
    if (is.null(cMethod)) {
      cMethod <- colnames(theObject@classification)
    }

    if (withAmplitudes) {
      df <- cbind(theObject@dropletAmplitudes,
                             theObject@classification[, cMethod])
      clCols <- seq_len(ncol(df))[-c(1,2)]
      colnames(df)[clCols] <- cMethod
      return(df)
    } else {
      return(theObject@classification[, cMethod])
    }
  }
)


#' @rdname wellClassification
#'
#' @export

setGeneric("wellClassification<-", function(theObject, cMethod, value) {
    standardGeneric("wellClassification<-")
  }
)

#' @rdname wellClassification
#'
#' @export

setReplaceMethod("wellClassification", "ddpcrWell",
  function(theObject, cMethod, value) {
    if(length(value) != numDroplets(theObject))
      stop("The length of 'value' should be the same as the number ",
           "of droplets in the well.")
    theObject@classification[, cMethod] <-
      factor(value, levels=ddpcr$classesRain)
    validObject(theObject)
    return(theObject)
  }
)


#' Retrieve the classification method.
#'
#' Retrieve the names of the classification methods for
#' a \code{\link{ddpcrWell}} object.
#'
#' @param theObject A \code{\link{ddpcrWell}} object.
#' @param cMethod If modifying the classification methods, this should be
#' a vector of existing classification names or numbers.
#' @param value A character vector (of the same length as \code{cMethod})
#' giving a new names for the chosen classification methods.
#'
#' @return The classification method names.
#'
#' @seealso \code{\link{wellClassification}} for the classification of the
#' droplets.
#'
#' @examples
#' ## Create a ddpcrWell object with some data and classification.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#'
#' ## Retrieve the classification method names.
#' head(wellClassificationMethod(aWell))
#'
#' ## Set a classification method name to something new.
#' wellClassificationMethod(aWell, cMethod="Cluster") <- "QuantaSoft"
#'
#' ## We check the classification now, showing the amplitudes as well.
#' wellClassificationMethod(aWell)
#'
#' @name wellClassificationMethod
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @export

setGeneric("wellClassificationMethod", function(theObject) {
  standardGeneric("wellClassificationMethod")
})

#' @rdname wellClassificationMethod
#'
#' @exportMethod wellClassificationMethod

setMethod("wellClassificationMethod", "ddpcrWell", function(theObject) {
    colnames(theObject@classification)
})


#' @rdname wellClassificationMethod
#'
#' @export

setGeneric("wellClassificationMethod<-", function(theObject, cMethod, value) {
  standardGeneric("wellClassificationMethod<-")
})

#' @rdname wellClassificationMethod
#'
#' @export

setReplaceMethod(
  "wellClassificationMethod", "ddpcrWell",
  function(theObject, cMethod, value) {
    if(is.character(cMethod)) {
      if(!cMethod %in% names(theObject@classification))
        stop("The parameter 'cMethod' is not an existing ",
             "classification method name.")
      n <- which(names(theObject@classification) %in% cMethod)
    } else {
      n <- cMethod
    }

    names(theObject@classification)[n] <- value
    validObject(theObject)
    return(theObject)
  }
)


#' @rdname ddpcrWell-class
#'
#' @inheritParams methods::show

setMethod("show", "ddpcrWell", function(object) {
  cat("ddpcrWell object\n")
  cat("number of droplets: ", numDroplets(object), "\n", sep="")
  if(!isEmpty(object)) {
    cat("classification methods:", wellClassificationMethod(object), "\n")
  }
})

