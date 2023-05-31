
#'Function that opens the shiny app, this is the only function that is exported and needs to be run to use the app
#'
#'@export
#'
#'@return the R shiny app

run_NBA_pbp <- function() {
  shiny::shinyApp(ui = ui, server = server)
}





