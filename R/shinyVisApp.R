#' Shiny app.
#'
#' A shiny app for interactive ddPCR droplet classification.
#'
#' @return A Shiny application
#'
#' @import shiny
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @export

shinyVisApp <- function() {
  shiny::shinyApp(
    ui=shinyVisUI(),
    server=function(input, output, session) {
      shinyVisServer(input, output, session)
    }
  )
}

