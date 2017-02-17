#' @title Droplet amplitude data for KRAS mutant and wild type molecules.
#'
#' @description A data frame of ddPCR droplet amplitudes using KRAS cell lines 
#' and pre-defined mutant/wild type ratios. The existing classification under 
#' the column name \code{Cluster} was obtained from Bio-Rad's QuantaSoft by 
#' setting the \code{Ch1.Amplitude} threshold to \code{6789} and the 
#' \code{Ch2.Amplitude} threshold to \code{3000}.
#'
#' @docType data
#'
#' @usage data(KRASdata)
#'
#' @format A list of data frames, each of which has columns 
#' \code{Ch1.Amplitude}, \code{Ch2.Amplitude} and \code{Cluster}.
#'
#' @return A data frame.
#'
#' @author From ddPCR experiments run by Mahmood Ayub, 
#' \email{mahmood.ayub@cruk.manchester.ac.uk}
#'
#' @keywords datasets

"KRASdata"

