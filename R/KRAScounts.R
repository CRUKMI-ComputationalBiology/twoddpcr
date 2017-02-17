#' @title KRAS mutant and wild type droplet counts and Poisson estimates.
#'
#' @description A data frame of droplet counts using the standard 
#' \code{Cluster} classification from \code{\link{KRASdata}}. Each row 
#' corresponds to a well/sample and the columns \code{PP}, \code{PN}, \code{NP} 
#' and \code{NN} show how many droplets were in each cluster. The remaining 
#' rows were calculated from the counts figures except for the 
#' \code{InputAmount} column.
#'
#' @details The data frame was created by:
#' \preformatted{krasPlate <- ddpcrPlate(wells=KRASdata)
#' KRAScounts <- plateSummary(krasPlate, cMethod="Cluster")
#' onesVector <- c(1, 1, 1)
#' runAmount <- c(64 * onesVector, 16 * onesVector, 4 * onesVector, onesVector)
#' KRAScounts$InputAmount <- runAmount
#' 
#' KRAScountsWellCol <- KRAScounts
#' KRAScountsWellCol$Well <- rownames(KRAScounts)
#' KRAScountsWellCol <- KRAScountsWellCol[, c(18,1:17)]
#' rownames(KRAScountsWellCol) <- NULL
#' }
#'
#' @docType data
#'
#' @usage data(KRAScounts)
#'
#' @format A data frame where each row corresponds to a well/sample.
#'
#' @return A data frame.
#'
#' @name KRAScounts
#'
#' @author From the \code{\link{KRASdata}} dataset created by Mahmood Ayub, 
#' \email{mahmood.ayub@cruk.manchester.ac.uk}
#'
#' @keywords datasets

"KRAScounts"


#' @rdname KRAScounts
#'
#' @description \code{KRAScountsWellCol} is the same data frame but has 
#' a \code{Well} column instead of named rows.
#'
#' @usage data(KRAScountsWellCol)

"KRAScountsWellCol"


#' @rdname KRAScounts
#'
#' @description \code{KRAScountsQS} is a data frame imported from a CSV created 
#' by Bio-Rad's QuantaSoft.
#'
#' @usage data(KRAScountsQS)

"KRAScountsQS"

