#' viewShiny
#'
#' @description
#' Open the shiny app
#'
#' @details
#' User specifies the shiny app location and this function opens it
#' 
#' @param shinyAppLocation  The folder to create the shiny app in                                       
#' @return
#' The shiny app will open
#'
#' @export
viewShiny <- function(shinyAppLocation){
  # check settings
  
  # open up the app
  shiny::runApp(shinyAppLocation) 
}



