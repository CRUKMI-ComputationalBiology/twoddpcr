#' @import ggplot2
NULL

#' Use a theme with a white background and grey lines.
#'
#' The default \code{\link[ggplot2]{ggplot}} theme has a grey background. This
#' layer can be added to change the aesthetics.
#'
#' @param legendPosition Where to position the legend. This can be "none",
#' "left", "right", "bottom", "top" or a numeric vector of length two. See the
#' documentation for \code{\link[ggplot2]{theme}}.
#' @param legendJustification The justification to use for the legend
#' positioning. This should be "center" or a two-element vector. Defaults to
#' "center".
#'
#' @return A \code{ggplot} theme layer.
#'
#' @seealso This function uses \code{\link[ggplot2]{theme}}.
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @examples
#' ## Get a data frame in the right format.
#' aWell <- KRASdata[["E03"]]
#' aWell$Cluster <- relabelClasses(aWell, classCol="Cluster")
#'
#' ## Plot it with the white theme.
#' ggplot2::ggplot(aWell, ggplot2::aes(x=Ch2.Amplitude, y=Ch1.Amplitude)) +
#'   ggplot2::geom_point(ggplot2::aes(colour=Cluster)) +
#'   whiteTheme()
#'
#' @export

whiteTheme <- function(legendPosition="right", legendJustification="center") {
  theme_bw(base_family="Helvetica") +
    theme(panel.grid.major=element_line(size=.5, color="grey"),
      axis.line=element_line(size=.7, color="black"),
      legend.position=legendPosition,
      legend.justification=legendJustification)
}

