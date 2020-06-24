#' launches the GPred19 app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function(x) {
  shinyApp(ui = shinyAppUI, server = shinyAppServer, options = list(host=x, port=6271))
}

