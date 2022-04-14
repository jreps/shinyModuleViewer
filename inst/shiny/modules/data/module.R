

dataViewer <- function(id=1) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::fluidRow(
      shiny::includeMarkdown(path = 'modules/data/data.md')
    )
    
  )
}

dataServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      
    }
  )
}
