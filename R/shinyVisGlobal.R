#' A Shiny environment.
#'
#' Stores modes used the in Shiny UI and internal defaults.
#'
#' @return An environment for storing Shiny variables.
#'
#' @export
shinyVis <- new.env()

# Modes.
shinyVis$classifyModes <- c("K-means Clustering", "Thresholds",
                            "Grid", "K-Nearest Neighbour")

# Thresholds for gating and all plots.
shinyVis$threshDefault <- list(ch1=7000, ch2=3500)
shinyVis$threshLimits <- list(ch1=list(min=3000, max=13500),
                              ch2=list(min=1000, max=9000))

