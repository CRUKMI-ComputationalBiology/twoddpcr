#' @include global.R
#' @include ddpcrWell.R
#' @import methods
#' @import stats
#' @import utils
#' @importClassesFrom S4Vectors SimpleList
#' @importFrom S4Vectors elementType
NULL


#' Check the types of the elements in a \code{SimpleList}.
#'
#' Check the types of the elements in a \code{SimpleList}.
#'
#' @param x An object.
#'
#' @return The types of objects in the \code{SimpleList} object.
#'
#' @export

setMethod("elementType", "SimpleList", function(x) x@elementType)


#' An S4 class for multiple wells in a ddPCR experiment.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @name ddpcrPlate-class
#'
#' @export ddpcrPlate
#' @exportClass ddpcrPlate

.ddpcrPlate <-
  setClass(
    "ddpcrPlate",
    contains="SimpleList",
    prototype=prototype(elementType="ddpcrWell"),
    validity=function(object) {
      # All elements of 'wells' should be 'ddpcrWell' objects.
      if(any(lapply(object, class) != "ddpcrWell")) {
        return("All wells should be 'ddpcrWell' objects.")
      }

      return(TRUE)
    }
  )


#' The constructor for the \code{ddpcrPlate} class.
#'
#' @param wells Either:
#' \itemize{
#'   \item a list of \code{\link{ddpcrWell}} objects,
#'   \item a list of data frames of droplet amplitudes, or
#'   \item a character vector corresponding to a file path(s) containing CSV
#'   files of raw droplet amplitude data.
#' }
#'
#' @return A \code{ddpcrPlate} object with the given wells.
#'
#' @rdname ddpcrPlate-class
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## A \code{ddpcrPlate} object can be created from data from a list of data
#' ## frames.
#' ddpcrPlate(KRASdata)
#'
#' ## A directory (or individual files) of droplet amplitude CSVs can also be
#' ## loaded.
#' moreAmpsDir <- system.file("extdata", "more-amplitudes", package="twoddpcr")
#' ddpcrPlate(moreAmpsDir)
#'
#' @export


setGeneric("ddpcrPlate", function(wells) {
  standardGeneric("ddpcrPlate")
})

#' @rdname ddpcrPlate-class
#'
#' @export

setMethod("ddpcrPlate", "list", function(wells) {
  # Try to make ddpcrWell objects from a list of data frames.
  if(all(vapply(wells, class, character(1)) == "data.frame")) {
    # Set the column names to the correct format, then coerce them.
    wells <- setChannelNames(wells)
    wells <- lapply(wells, ddpcrWell)
  }

  setNames(.ddpcrPlate(listData=wells), names(wells))
}
)

#' @rdname ddpcrPlate-class
#'
#' @export

setMethod("ddpcrPlate", "ddpcrPlate", function(wells) {
  wells <- as.list(wells)
  ddpcrPlate(wells)
})

#' @rdname ddpcrPlate-class
#'
#' @export

setMethod("ddpcrPlate", "character", function(wells) {
  # Character string wells: hopefully a directory path.
  # File or directory path---read it.
  if(all(file.exists(wells))) {
    wells <- readCSVDataFrame(wells)
    if(length(wells) == 0) {
      warning("No droplet amplitudes loaded. Was this intended?")
    }
    ddpcrPlate(lapply(wells, ddpcrWell))
  } else {
    stop("The string 'wells' is not a file or directory path.")
  }
}
)

#' @rdname ddpcrPlate-class
#'
#' @export

setMethod("ddpcrPlate", "missing", function(wells) {
  .ddpcrPlate(listData=list())
})


#' @rdname amplitudes
#'
#' @exportMethod amplitudes

setMethod("amplitudes", "ddpcrPlate", function(theObject) {
  if(length(theObject) == 0) {
    return(data.frame("Ch1.Amplitude"=double(),
                      "Ch2.Amplitude"=double()))
  } else {
    return(lapply(theObject, amplitudes))
  }
})


#' Splits a long vector and according to a vector of sizes.
#'
#' Takes one long vector/factor and splits it into a list of vectors, where the
#' lengths are given by a vector of sizes. This may be the same as another
#' given list of vectors. Particularly useful if we want to combine a list of
#' data, do some analysis on the combined data, then split the analysis.
#'
#' @param vec The vector to split.
#' @param wellSizes A numeric vector corresponding to sizes of the wells.
#' @param wellNames A character vector corresponding to the names of the wells.
#'
#' @return A list of vectors split into the given lengths.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.slice <- function(vec, wellSizes, wellNames) {
  sliced <- setNames(vector("list", length(wellNames)), wellNames)
  i <- 1
  for(j in seq_along(wellSizes)) {
    s <- wellSizes[j]
    sliced[[wellNames[j]]] <- vec[i:(i+s-1)]
    i <- i + s
  }
  sliced
}


#' Retrieve the well names to use from a given list.
#'
#' We simply retrieve the \code{names} from a given list, but also perform some
#' checks to make sure that the names are consistent with a given
#' \code{\link{ddpcrPlate}} object.
#'
#' @param theObject A \code{\link{ddpcrPlate}} object.
#' @param aList A list from which we wish to extract well names.
#'
#' @return The names of \code{aList}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.extractWellNames <- function(theObject, aList) {
  # Set well names.
  if(!all(is.null(names(aList))) && all(names(aList) %in% names(theObject))) {
    wells <- names(aList)
  } else {
    stop("Not all names specified in the list 'aList' ",
         "are well names in 'theObject'.")
  }

  # Inconsistencies between the given list and wells.
  if(!all(wells %in% names(theObject))) {
    stop("Some of the given 'wells' are not valid well names in ",
         "'theObject'.")
  }

  wells
}


#' Set and retrieve classifications for multiple wells.
#'
#' Retrieve multiple classification factors that have been assigned to
#' a \code{ddpcrPlate} object.
#'
#' @param theObject A \code{\link{ddpcrPlate}} object.
#' @param cMethod This is the name of the classification to retrieve and should
#' be a character vector. If \code{NULL}, then all of the classifications are
#' obtained. Defaults to \code{NULL}.
#' @param value Either:
#' \itemize{
#'   \item A list of factors, where each item of the list corresponds to
#'   a well;
#'   \item A single factor corresponding to all of the wells combined. This
#'   should be ordered by the order of the output of the
#'   \code{\link{amplitudes}} function when the rows of the data frames have
#'   been bound together, i.e. with \code{do.call(rbind,
#'   amplitudes(theObject))}.
#' }
#' @param withAmplitudes If \code{TRUE}, the droplet amplitudes are included.
#' Defaults to \code{FALSE}.
#' @param wellCol If \code{TRUE}, an additional column is included in the
#' output, where each entry is the name of the well from which the droplet
#' originated. In this case, this setting forces the \code{withAmplitudes}
#' parameter to \code{TRUE}. Defaults to \code{FALSE}.
#'
#'
#' @return If requesting one classification without the amplitudes, a list of
#' factors corresponding to the classifications is returned. Otherwise, a list
#' of data frames is returned where each row corresponds to a droplet in the
#' corresponding well.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ### The examples here show how this method works by setting classifications
#' ### using data frames. To do this, we use the
#' ### \code{\link{thresholdClassify}} method on _data frames_. Note that
#' ### \code{thresholdClassify} also works directly on \code{ddpcrWell} and
#' ### \code{ddpcrPlate} objects; this is simply an illustration of
#' ### how to use the \code{plateClassification} method directly. In general,
#' ### it is recommended to use \code{thresholdClassify} directly on
#' ### \code{ddpcrPlate} objects.
#'
#' ## Create a ddpcrPlate object.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#'
#' ## Classify a data frame of droplets and keep it in a _single_ data frame.
#' ## Set the new classification from this.
#' droplets <- do.call(rbind, amplitudes(krasPlate))
#' clSingle <- thresholdClassify(droplets,
#'                               ch1Threshold=7000, ch2Threshold=3500,
#'                               fullTable=FALSE)
#' plateClassification(krasPlate, "thresholdSing") <- clSingle
#'
#' ## We can also set the new classification from a list of factors.
#' clList <- lapply(KRASdata, thresholdClassify, ch1Threshold=7000,
#'                  ch2Threshold=3500, fullTable=FALSE)
#' plateClassification(krasPlate, "thresholdList") <- clList
#'
#' ## We can get all of the classifications as a list of data frames.
#' plate <- plateClassification(krasPlate)
#' lapply(plate, head, n=1)
#'
#' ## We can include the droplet amplitudes columns.
#' plate <- plateClassification(krasPlate, withAmplitudes=TRUE)
#' lapply(plate, head, n=1)
#'
#' ## We can focus on specific classifications.
#' plate <- plateClassification(krasPlate, cMethod=c("thresholdSing",
#'                                                   "thresholdList"))
#' lapply(plate, head, n=1)
#'
#' ## The wellCol option adds an extra column showing which well the droplet
#' ## came from.
#' plate <- plateClassification(krasPlate, withAmplitudes=TRUE, wellCol=TRUE)
#' lapply(plate, head, n=1)
#'
#'
#' @name plateClassification
#'
#' @export

setGeneric(
  "plateClassification",
  function(theObject, cMethod=NULL, withAmplitudes=FALSE, wellCol=FALSE) {
    standardGeneric("plateClassification")
  }
)

#' @rdname plateClassification
#'
#' @exportMethod plateClassification

setMethod("plateClassification", "ddpcrPlate",
  function(theObject, cMethod=NULL, withAmplitudes=FALSE, wellCol=FALSE) {
    # Force withAmplitudes to be true if we're including a well column.
    if(wellCol && !withAmplitudes) {
      withAmplitudes <- TRUE
    }

    mc <- lapply(theObject, wellClassification,
                 withAmplitudes=withAmplitudes, cMethod=cMethod)

    # Retrieve the classification. Simplify to a factor if one classification
    # has been requested without amplitudes; use data frames otherwise.
    # if(!withAmplitudes && (length(cMethod) == 1))

    # Add a well column to show where the droplet originated.
    if(wellCol) {
      wellNames <- names(theObject)
      mc <- lapply(
        wellNames, function(w) {
          data.frame(mc[[w]], "Well"=factor(w, levels = unique(wellNames)))
        }
      )
      names(mc) <- wellNames
    }

    mc
  }
)

#' @rdname plateClassification
#'
#' @export

setGeneric("plateClassification<-", function(theObject, cMethod, value) {
    standardGeneric("plateClassification<-")
  }
)

#' @rdname plateClassification
#'
#' @export

setReplaceMethod("plateClassification", c("ddpcrPlate", "character", "list"),
  function(theObject, cMethod, value) {
    # Basic checks on the well names.
    wells <- .extractWellNames(theObject, value)
    wellDropletCount <- vapply(
      wells, function(w) numDroplets(theObject[[w]]), numeric(1)
    )
    if(any(vapply(value, length, numeric(1)) != wellDropletCount)) {
      stop("The number of elements in the list items do not match ",
           "those of the existing wells of the same name.")
    }

    for(i in seq_along(wells)) {
      w <- wells[i]
      wellClassification(theObject[[w]], cMethod=cMethod) <- value[[i]]
    }

    validObject(theObject)
    return(theObject)
  }
)

#' @rdname plateClassification
#'
#' @export

setReplaceMethod("plateClassification", c("ddpcrPlate", "character", "factor"),
  function(theObject, cMethod, value) {
    # The classification 'value' is one long factor, so should be for the
    # combined wells.

    # Check that the vector length is correct.
    multiNumDroplets <- numDroplets(theObject)
    if(length(value) != sum(multiNumDroplets)) {
      stop("The length of 'vec' is not the same as the sum of 'wellSizes'.")
    }

    # Slice it.
    cl <- .slice(value, multiNumDroplets, names(theObject))

    # Set the classification.
    for(w in names(theObject)) {
      wellClassification(theObject[[w]], cMethod=cMethod) <- cl[[w]]
    }

    validObject(theObject)
    return(theObject)
  }
)


#' Set or retrieve the classification method strings for multiple wells.
#'
#' \code{plateClassificationMethod} retrieves multiple classification methods
#' that have been assigned to a \code{ddpcrPlate} object.
#'
#' @param theObject A \code{\link{ddpcrPlate}} object.
#' @param cMethod This should represent existing classification method(s) for
#' all wells in \code{theObject}. It can be given in the form of a:
#' \itemize{
#'   \item Character vector. If this vector is shorter than the length of
#'   \code{wells}, this vector's elements will be repeated.
#'   \item List of character vectors.
#' }
#' @param value New classification method(s) in the same form as
#' \code{cMethod}.
#'
#' @return \code{plateClassificationMethod} returns a list of character strings
#' corresponding to the classification methods.
#'
#' @seealso \code{\link{plateClassification}} for the classifications.
#'
#' @aliases plateClassName plateClassificationName plateClassMethod
#'
#' @rdname plateClassificationMethod
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get the classification methods for the KRASdata dataset.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' plateClassificationMethod(krasPlate)
#'
#' ## Change the "Cluster" column to "QS".
#' plateClassificationMethod(krasPlate, "Cluster") <- "QS"
#' plateClassificationMethod(krasPlate)
#'
#' ## Usually, all of the classification names are the same for all wells. We
#' ## use the \code{commonClassificationMethod} method to retrieve the ones
#' ## common to all wells.
#' commonClassificationMethod(krasPlate)
#'
#' @export

setGeneric("plateClassificationMethod", function(theObject) {
  standardGeneric("plateClassificationMethod")
})

#' @rdname plateClassificationMethod
#'
#' @exportMethod plateClassificationMethod

setMethod("plateClassificationMethod", "ddpcrPlate", function(theObject) {
  wellNames <- names(theObject)
  setNames(lapply(theObject, wellClassificationMethod), wellNames)
})

#' @rdname plateClassificationMethod
#'
#' @export

setGeneric("plateClassificationMethod<-", function(theObject, cMethod, value) {
  standardGeneric("plateClassificationMethod<-")
})

#' @rdname plateClassificationMethod
#'
#' @export

setReplaceMethod(
  "plateClassificationMethod", "ddpcrPlate",
  function(theObject, cMethod, value) {
    if(is.list(cMethod)) {
      cMethod <- unlist(cMethod)
    }
    if(is.list(value)) {
      value <- unlist(value)
    }

    if(is.vector(cMethod) && is.vector(value)) {
      wells <- names(theObject)

      # Set classification method for all wells, repeating elements of
      # cMethod if necessary.
      for(i in seq_along(wells)) {
        w <- wells[[i]]
        cmWrap <- cMethod[[(i - 1) %% length(cMethod) + 1]]
        wellClassificationMethod(theObject[[w]], cmWrap) <-
          value[[(i - 1) %% length(value) + 1]]
      }
    } else {
      stop("'cMethod' and 'value' should be vectors or lists ",
           "of indices or character strings.")
    }

    validObject(theObject)
    return(theObject)
  }
)


#' @rdname plateClassificationMethod
#'
#' @description \code{commonClassificationMethod} retrieves the classification
#' methods common to all the wells in the given \code{ddpcrPlate} object.
#'
#' @return \code{commonClassificationMethod} returns a vector of character
#' strings indicating which classification methods appear in all wells.
#'
#' @export

setGeneric("commonClassificationMethod", function(theObject) {
  standardGeneric("commonClassificationMethod")
})

#' @rdname plateClassificationMethod
#'
#' @exportMethod commonClassificationMethod

setMethod("commonClassificationMethod", "ddpcrPlate", function(theObject) {
  if(length(theObject) == 0) {
    return(0)
  } else {
    x <- plateClassificationMethod(theObject)
    common <- x[[1]]
    for(i in seq_along(x)) {
      common <- intersect(common, x[[i]])
    }
    return(common)
  }
}
)


#' @rdname ddpcrPlate-class
#'
#' @inheritParams methods::show

setMethod("show", "ddpcrPlate", function(object) {
  cat("ddpcrPlate object\n")
  cat("number of wells: ", length(object), "\n", sep="")
  if(!isEmpty(object)) {
    cat("well names: ", sep="")
    maxNumWells <- 10
    wellNames <- names(object)
    if(length(object) <= maxNumWells) {
      cat(wellNames, "\n", sep=" ")
    } else {
      remNumWells <- length(object) - maxNumWells
      cat(wellNames[seq(maxNumWells)],
          "...\n            (plus", remNumWells, "wells omitted)\n", sep=" ")
    }
  }
})
