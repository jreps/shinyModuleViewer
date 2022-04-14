library(dplyr)

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  
  #=============
  # sidebar menu
  #=============
  
  output$sidebarMenu <- shinydashboard::renderMenu(
    shinydashboard::sidebarMenu(
      id ='menu',
      
      addInfo(
        shinydashboard::menuItem(
          text = "About", 
          tabName = "About", 
          icon = shiny::icon("info")
        ), 
        "About"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          "Data", 
          tabName = "Data", 
          icon = shiny::icon("database")
        ), 
        "Data"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          "Prediction", 
          tabName = "Prediction", 
          icon = shiny::icon("table")
        ), 
        "Prediction"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          "Estimation", 
          tabName = "Estimation", 
          icon = shiny::icon("question")
        ), 
        "Estimation"
      )
      

    )
  )

  #=============
  # Helper
  #=============
  
  shiny::observeEvent(input$DescriptionInfo, {
    showInfoBox("About", "www/About.html")
  })
  shiny::observeEvent(input$ModelInfo, {
    showInfoBox("Prediction", "www/Prediction.html")
  })
  shiny::observeEvent(input$DataInfoInfo, {
    showInfoBox("Data", "www/Data.html")
  })
  
  #=============
  # module severs
  #=============
  aboutServer(id = 'about')
  
  dataServer(id = 'data')
  
  # run the module when it is selected the first time only
  viewPrediction <- shiny::reactiveVal(0)
  shiny::observeEvent(input$menu,{
    
    if(input$menu == 'Prediction'){
      newVal <- viewPrediction() + 1
      viewPrediction(newVal)
    }
    
    if(input$menu == 'Prediction' & viewPrediction()==1){ 
      predictionServer(
        id = 'prediction', 
        resultDatabaseSettings = list(
          myPort = keyring::key_get("myPort", "lungcancer"),
          targetDialect = keyring::key_get("targetDialect", "lungcancer"),
          myServer = keyring::key_get("myServer", "lungcancer"),
          myUser = keyring::key_get("myUser", "lungcancer"),
          myPassword = keyring::key_get("myPassword", "lungcancer"),
          
          mySchema = keyring::key_get("mySchema", "lungcancer"),
          myTableAppend =  'lungcancer_'
          
        ))
    }
  }
  )
  
  estimationServer(id = 'estimation')

}
)

# helper

addInfo <- function(item, infoId) {
  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button", 
    id = infoId,
    "i"
  )
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

showInfoBox <- function(title, htmlFileName) {
  shiny::showModal(shiny::modalDialog(
    title = title,
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
  ))
}
  