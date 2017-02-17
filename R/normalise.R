#' @import stats
NULL

#' K-means classify a data frame where the droplets are negative in the same
#' channels only.
#'
#' @param df A data frame corresponding to a well with droplets corresponding 
#' only to "NN" and "NP" or "NN" and "PN".
#' @param channel The channel on which to classify (1 or 2).
#' @param minSeparation The minimum distance required between two cluster
#' centres in order for us to assume that k-means found two distinct clusters. 
#' Defaults to 2000.
#' @param centres A data frame of centres. The data frame should have columns
#' \code{Ch1.Amplitude} and \code{Ch2.Amplitude} and row names corresponding
#' the cluster label, e.g. "NN", "NP", "PN" or "PP".
#'
#' @return A classification for \code{df}.
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @importFrom stats dist

.classifyDfOnChannel <- function(df, channel, centres=NULL,
                                 minSeparation=2000, fullTable=TRUE)
{
  # K-means classify with two clusters.
  km <- kmeansClassify(df, centres=centres, fullTable=fullTable)

  # If the cluster centres are too close, reject the classification
  # and use the original one.
  if(dist(km$centres[, channel]) >= minSeparation)
    km$data
  else
  {
    if(fullTable)
    {
      colnames(df)[3] <- "class"
      df
    }
    else
      df$kmeans
  }
}


#' K-means classify a list of data frames individually, where each data frame
#' comprises droplets that are negative in the same channels only.
#'
#' @param cl List of data frames, where each data frame corresponds to a well
#' with droplets corresponding only to "NN" and "NP" or "NN" and "PN".
#' @param channel The channel on which to classify (1 or 2).
#' @param minSeparation The minimum distance required between two cluster
#' centres in order for us to assume that k-means found two distinct clusters. 
#' Defaults to 2000.
#' @param centres A data frame of centres. The data frame should have columns
#' \code{Ch1.Amplitude} and \code{Ch2.Amplitude} and row names corresponding
#' the cluster label, e.g. "NN", "NP", "PN" or "PP".
#'
#' @return A classification for \code{cl}.
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.classifyOnChannel <- function(cl, channel, centres=NULL, minSeparation=2000, 
                               fullTable=TRUE)
{
  if(is.null(centres))
  {
    if(channel == 1)
    {
      centres <- data.frame(Ch1.Amplitude=c(0, 10000), Ch2.Amplitude=c(0, 0))
      rownames(centres) <- c("NN", "PN")
    }
    else if(channel == 2)
    {
      centres <- data.frame(Ch1.Amplitude=c(0, 0), Ch2.Amplitude=c(0, 7000))
      rownames(centres) <- c("NN", "NP")
    }
  }
  
  lapply(cl, .classifyDfOnChannel, channel, centres, minSeparation, fullTable)
}


#' Normalise a well on one channel only and then transform it back to the 
#' original (combined) scale.
#'
#' @param wellDf A data frame of the well's droplet amplitudes.
#' @param combinedCentres A data frame of the combined (average) centres of the 
#' non-normalised wells.
#' @param wellCentres A data frame of centres corresponding to the given 
#' \code{channel}.
#' @param channel An integer 1 or 2 corresponding to the channel that we are 
#' interested in.
#' 
#' @return A data frame with the rescaled amplitudes in the chosen channel.
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.renormaliseByChannel <- function(wellDf, combinedCentres, wellCentres, 
                                  channel)
{
  if(!channel %in% c(1, 2))
    stop("The parameter 'channel' should be 1 or 2.")
  
  # Select the well.
  s <- wellDf
  
  # Positive and negative cluster labels.
  n <- ddpcr$nn
  if(channel == 1)
  {
    ch <- "Ch1.Amplitude"
    p <- ddpcr$pn
  }
  else if(channel == 2)
  {
    ch <- "Ch2.Amplitude"
    p <- ddpcr$np
  }

  # Set transformation factors if they don't exist.
  if(is.na(wellCentres[n, ch]))
    wellCentreN <- combinedCentres[n, ch]
  else
    wellCentreN <- wellCentres[n, ch]

  if(is.na(wellCentres[p, ch]))
    wellCentreP <- combinedCentres[p, ch]
  else
    wellCentreP <- wellCentres[p, ch] 

  # Transform.
  translFactor <- wellCentreN
  scaleFactor <- wellCentreP - wellCentreN
  rescaleFactor <- combinedCentres[p, ch] - combinedCentres[n, ch]
  retranslFactor <- combinedCentres[n, ch]
  
  s[, ch] <- s[, ch] - translFactor
  s[, ch] <- s[, ch] / scaleFactor
  s[, ch] <- s[, ch] * rescaleFactor
  s[, ch] <- s[, ch] + retranslFactor

  s
}


#' Normalise a well in both channels and then transform it back to the original 
#' (combined) scale.
#'
#' @param allDf A list of data frames, where each one corresponds to a well's 
#' droplet amplitudes.
#' @param well The name or number of the well to normalise.
#' @param combinedCentres A data frame of the combined (average) centres of the 
#' non-normalised wells.
#' @param indivCentres1 A data frame of centres corresponding to channel 1.
#' @param indivCentres2 A data frame of centres corresponding to channel 2.
#' 
#' @return A list of data frames with the rescaled amplitudes in both channels.
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.renormaliseWell <- function(allDf, well, combinedCentres,
                             indivCentres1, indivCentres2)
{
  s <- allDf[[well]]
  s <- .renormaliseByChannel(s, combinedCentres, indivCentres2[[well]], 
                             channel=2)
  .renormaliseByChannel(s, combinedCentres, indivCentres1[[well]], channel=1)
}


#' Find the centres of each of the wells in a given channel.
#' 
#' The plate should have been classified by k-means clustering.
#' 
#' @param plate A \code{ddpcrPlate} object from which to extract the centres.
#' @param cMethod The classification method to use (in the form of a character 
#' string).
#' @param channel An integer 1 or 2 corresponding to the channel of interest.
#' @param minSeparation The minimum distance required between two cluster
#' centres in order for us to assume that k-means found two distinct clusters. 
#' Defaults to 2000.
#' 
#' @return A list of data frames, where each data frame has information about 
#' the centres in the channel of interest.
#' 
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}

.getChannelCentres <- function(plate, cMethod, channel, minSeparation=2000)
{
  if(channel == 1)
    toRemove <- c("NP", "PP")
  else if(channel == 2)
    toRemove <- c("PN", "PP")
  else
    stop("The parameter 'channel' should be 1 or 2.")
  
  # Remove the unneeded droplets.
  cl <- removeDropletClasses(plate, cMethod=cMethod, classesToRemove=toRemove)
  cl <- .classifyOnChannel(cl, channel=channel, minSeparation=minSeparation)
  clusterCentres(ddpcrPlate(cl), cMethod="class")
}


#' @title Normalise all wells in a plate to the overall/average cluster 
#' positions.
#' 
#' @description Each well is scaled in the direction of the two axes 
#' independently. The algorithm works as follows:
#' \enumerate{
#'   \item Classify the entire plate using k-means clustering in order to 
#'     roughly identify clusters.
#'   \item For each channel and each well:
#'     \enumerate{
#'       \item Remove the positive clusters in the other channel.
#'       \item Re-run k-means clustering with \eqn{k = 2} to obtain new
#'         cluster centres. If the centres are too close (default distance
#'         2000), reject these new cluster centres and use the old centres.
#'       \item Use the new centres for each well to rescale to the same scale 
#'       as the average/overall scale.
#'     }
#' }
#'
#' @param plate A \code{ddpcrPlate} object.
#' @param initialCentres A matrix or data frame of (rough estimates) of centres 
#' of each of the clusters in the combined data. This will be used for an 
#' initial run of k-means clustering.
#' @param minSeparation The minimum distance required between two cluster
#' centres in order for us to assume that k-means clustering found two distinct 
#' clusters. This is used when classifying droplets along a single channel and 
#' helps to reject classifications where there is only one cluster, e.g. when 
#' there are no mutants in a well. Defaults to 2000.
#' 
#' @return A list of data frames with the rescaled amplitudes in both channels.
#'
#' @name renormalisePlate
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#' 
#' @examples
#' ## Normalise the KRAS data.
#' plate <- ddpcrPlate(wells=KRASdata)
#' normPlate <- renormalisePlate(plate)
#'
#' @export

renormalisePlate <- function(plate,
                             initialCentres=matrix(c(0, 0, 10000, 0,
                                                     0, 7000, 10000, 7000),
                                                   ncol=2, byrow=TRUE),
                             minSeparation=2000)
{
  # Classify everything first and get the combined centres.
  plate <- kmeansClassify(plate, centres=initialCentres)
  combCentres <- combinedCentres(plate, cMethod="kmeans")

  # Get the centres for the WT/Mut-only classification
  indivCentres1 <- .getChannelCentres(plate, cMethod="kmeans", channel=1, 
                                      minSeparation=minSeparation)
  indivCentres2 <- .getChannelCentres(plate, cMethod="kmeans", channel=2,
                                      minSeparation=minSeparation)

  # Get all of the droplet amplitudes.
  clAll <- plateClassification(plate, cMethod="kmeans", withAmplitudes=TRUE)

  # Renormalise each of the wells.
  normAll <- lapply(names(clAll),
                    .renormaliseWell, allDf=clAll, combinedCentres=combCentres,
                    indivCentres1=indivCentres1, indivCentres2=indivCentres2)
  
  ddpcrPlate(setNames(normAll, names(clAll)))
}

