#' @import methods
#' @importFrom S4Vectors isEmpty
NULL

#' Retrieve the number of droplets.
#'
#' Retrieves the number of droplets in a \code{\link{ddpcrWell}} or 
#' \code{\link{ddpcrPlate}} object.
#' 
#' @param theObject A \code{\link{ddpcrPlate}} object.
#' @param ... Other parameters depending on the type of \code{theObject}.
#' 
#' @return For \code{ddpcrWell} objects, return the number of droplets as an 
#' integer.
#'
#' @name numDroplets
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @aliases numberDroplets numDrops numberDrops numberOfDroplets numOfDroplets 
#' numberOfDrops
#'
#' @examples
#' ## Count the number of droplets in a well.
#' aWell <- ddpcrWell(well=KRASdata[["E03"]])
#' numDroplets(aWell)
#'
#' @export

setGeneric("numDroplets", function(theObject, ...)
  {
    standardGeneric("numDroplets")
  }
)

#' @rdname numDroplets
#'
#' @exportMethod numDroplets

setMethod("numDroplets", "ddpcrWell", function(theObject)
  {
    nrow(theObject@dropletAmplitudes)
  }
)


#' @rdname numDroplets
#'
#' @return For \code{ddpcrPlate} objects, return a named vector. The names 
#' correspond to a well name and each item corresponding to the number of 
#' droplets in that well.
#'
#' @examples
#' ## Get all of the wells in a named vector.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' (numberDroplets <- numDroplets(krasPlate))
#' sum(numberDroplets)
#'
#' ## We can choose to get a subset of the wells.
#' (numberDroplets <- numDroplets(krasPlate[c("H03", "A04")]))
#' sum(numberDroplets)
#'
#' @exportMethod numDroplets

setMethod("numDroplets", "ddpcrPlate", function(theObject)
  {
    # Handle the special case where there are no wells.
    if(length(theObject) == 0)
      return(c())

    vapply(theObject, numDroplets, numeric(1))
  }
)


#' Is a \code{\link{ddpcrWell}} object empty?
#' 
#' Returns a logical value as to whether the given object has no 
#' droplets/wells.
#' 
#' @param x An object to test for emptiness.
#' 
#' @return A logical value.
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Check that an empty ddpcrPlate object is in fact empty.
#' krasPlate <- ddpcrPlate(wells=list())
#' isEmpty(krasPlate)
#'
#' ## Now add some amplitude data and check that is not empty.
#' krasPlate <- ddpcrPlate(wells=KRASdata)
#' isEmpty(krasPlate)
#'
#' @rdname ddpcrWell-methods
#'
#' @exportMethod isEmpty

setMethod("isEmpty", "ddpcrWell", function(x)
  {
    if(numDroplets(x) == 0)
      return(TRUE)
    else
      return(FALSE)
  }
)


#' @rdname ddpcrWell-methods
#'
#' @exportMethod isEmpty

setMethod("isEmpty", "ddpcrPlate", function(x)
  {
    if(length(x) == 0)
      return(TRUE)
    else
      return(sum(numDroplets(x)) == 0)
  }
)


