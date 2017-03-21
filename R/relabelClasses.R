#' @import stats
NULL

#' Re-label clusters.
#'
#' An attempt to label clusters with "NN", "NP", "PN" or "PP" automatically. 
#' (This will not generalise to amplitudes in only a single channel.)
#'
#' @param droplets A data frame of droplet amplitudes with a classification.
#' @param classCol The column (name or number) from 'droplets' representing 
#' the class.
#' @param presentClasses A vector of classes that we want to label. Must be 
#' a subset of c("NN", "NP", "PN", "PP") and must have the same number of 
#' classes as the number of unique classes in the class column.
#'
#' @return The classification column relabelled with "NN", "NP", "PN" and "PP".
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Look at the "Cluster" column that was created by Bio-Rad's QuantaSoft.
#' aWell <- KRASdata[["E03"]]
#' str(aWell$Cluster)
#'
#' ## Relabel the classes to see the difference.
#' relabelled <- relabelClasses(aWell, classCol="Cluster")
#' str(relabelled)
#' levels(relabelled)
#'
#' ## We choose a sample with 3 clusters.
#' unique(KRASdata[["H04"]]$Cluster) 
#'
#' ## We can check that there is no "PP" class, so specify the others only.
#' relabelled <- relabelClasses(KRASdata[["H04"]], classCol="Cluster",
#'                              presentClasses=c("NN", "PN", "NP"))
#' table(relabelled)
#'
#' @export

relabelClasses <- function(droplets, classCol="class",
                          presentClasses=ddpcr$classes)
{
  # Everything is easier if we use the column name instead of number.
  if(is.numeric(classCol))
    classCol <- colnames(droplets)[classCol]
  
  # Only one class: label everything as given by presentClasses.
  if(length(unique(droplets[, classCol])) == 1)
  {
    newClassification <- rep(presentClasses[1], length(droplets[, classCol]))
    return(factor(newClassification, levels=ddpcr$classesRain))
  }
      
  # Bottom-left of each cluster with a shift-factor to the origin.
  bl <- stats::aggregate(
    droplets[!droplets[, classCol] %in% c(ddpcr$rain, ddpcr$na),
             c("Ch1.Amplitude", "Ch2.Amplitude")],
    by=list("class"=droplets[!droplets[, classCol] %in% 
            c(ddpcr$rain, ddpcr$na), classCol]),
    FUN=min)
  ch1ShiftFactor <- min(bl$Ch1.Amplitude)
  ch2ShiftFactor <- min(bl$Ch2.Amplitude)
  
  # Top-right of each cluster with a scaling factor.
  tr <- stats::aggregate(
    droplets[!droplets[, classCol] %in% c(ddpcr$rain, ddpcr$na),
    c("Ch1.Amplitude", "Ch2.Amplitude")],
    by=list("class"=droplets[!droplets[, classCol] %in% 
            c(ddpcr$rain, ddpcr$na), classCol]),
    FUN=max)
  ch1NormFactor <- max(tr$Ch1.Amplitude) - ch1ShiftFactor
  ch2NormFactor <- max(tr$Ch2.Amplitude) - ch2ShiftFactor
  
  # Normalise everything.
  tr$Ch1.Amplitude <- (tr$Ch1.Amplitude - ch1ShiftFactor) / ch1NormFactor
  tr$Ch2.Amplitude <- (tr$Ch2.Amplitude - ch2ShiftFactor) / ch2NormFactor
  bl$Ch1.Amplitude <- (bl$Ch1.Amplitude - ch1ShiftFactor) / ch1NormFactor
  bl$Ch2.Amplitude <- (bl$Ch2.Amplitude - ch2ShiftFactor) / ch2NormFactor
  
  # Bottom-right and top-left.
  br <- data.frame("Ch1.Amplitude"=bl$Ch1.Amplitude,
                   "Ch2.Amplitude"=tr$Ch2.Amplitude,
                   "class"=bl$class)
  tl <- data.frame("Ch1.Amplitude"=tr$Ch1.Amplitude,
                   "Ch2.Amplitude"=bl$Ch2.Amplitude,
                   "class"=bl$class)
  
  # Score the clusters according to their location. Basic idea here: transform 
  # the candidate cluster to the bottom left so that they are the closest to 
  # the origin; the cluster with the smallest sup.dist from the origin is the 
  # best candidate.
  tr$sup.dist <- mapply(max, 1 - tr$Ch1.Amplitude, 1 - tr$Ch2.Amplitude)
  tl$sup.dist <- mapply(max, 1 - tl$Ch1.Amplitude, tl$Ch2.Amplitude)
  br$sup.dist <- mapply(max, br$Ch1.Amplitude, 1 - br$Ch2.Amplitude)
  bl$sup.dist <- mapply(max, bl$Ch1.Amplitude, bl$Ch2.Amplitude)
  distances <- data.frame("class"=tr$class, "NN"=bl$sup.dist, "NP"=br$sup.dist,
                          "PN"=tl$sup.dist, "PP"=tr$sup.dist)
  colnames(distances)[2:5] <- ddpcr$classes
  distances <- distances[, c("class", presentClasses)]
  
  # Labelling process.
  # Repeat the following for the original number of clusters:
  #   Find the smallest number in each column of 'distances'; choose the (row, 
  #   column) with the smallest such min.
  #   Assign this column's label to the class corresponding to this row.
  #   Remove the entire row and column from 'distances'.
  labels <- setNames(vector("list", ncol(distances) + 2),
                     c(presentClasses, ddpcr$rain, ddpcr$na))
  for(n in seq_len(nrow(distances)))
  {
    min.row <- which.min(apply(distances, MARGIN=1, min))
    min.col <- which.min(apply(distances, MARGIN=2, min))
    labels[[as.character(distances[min.row, "class"])]] <- 
      colnames(distances)[min.col]
    distances <- distances[-min.row, -min.col]
  }
  labels[[ddpcr$rain]] <- ddpcr$rain
  labels[[ddpcr$na]] <- ddpcr$na
  
  # Rename the original classes to the new class names.
  renamed <- factor(unlist(labels[as.character(droplets[, classCol])]),
                    levels=ddpcr$classesRain)
  setNames(renamed, NULL)
}


