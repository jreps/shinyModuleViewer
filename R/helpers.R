viewShiny <- function(shinyAppLocation){
  # check settings
  
  # open up the app
  shiny::runApp(shinyAppLocation) 
}


if(F){
  # testing
loc <- '/Users/jreps/Documents/github/shinyModuleViewer/inst/shiny/config.json'
config <-  ParallelLogger::loadSettingsFromJson(loc)
res <- createUiText(config)
res <- createServerText(config)

createShinyFiles(config, shinyAppLocation = '/Users/jreps/Documents/shinyTest')



}


