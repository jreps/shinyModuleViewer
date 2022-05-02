createShinyApp <- function(
  config, 
  shinyAppLocation, 
  overwrite = F
  ){
  
  # create the shinyAppLocation
  if(dir.exists(shinyAppLocation) & overwrite == F){
    stop('shinyApp Location exists - please use a different location or set overwrite to T')
  }
    
  if(!dir.exists(shinyAppLocation)){
    dir.create(shinyAppLocation, recursive = T)
  }
  
  # download the modules based on the config
  downloadModules(
    config = config, 
    shinyAppLocation = shinyAppLocation
  )
  
  # create the UI and server files
  createShinyFiles(
    config = config, 
    shinyAppLocation = shinyAppLocation
    )
  
  return(shinyAppLocation)
}




# helpers
downloadModules <- function(
  config, 
  shinyAppLocation
){
  
  tempLocation <- tempdir()
  
  for(moduleConfig in config$shinyModules){
    
    user <- moduleConfig$user
    repository <- moduleConfig$repository
    branch <- moduleConfig$branch
    moduleName <- moduleConfig$moduleName
    
    ParallelLogger::logInfo(paste0('Downloading shiny module ', moduleName, '  from ', user, '/', repository))
    
    # download zipped file into file.path(shinyAppLocation, 'modules')
    url <- paste0(
      'https://raw.github.com/',
      user,
      '/',
      repository,
      '/',
      branch,
      '/inst/shiny/modules/',
      paste0(moduleName,'.zip')
    )
    
    download.file(
      url = url, 
      destfile = file.path(
        tempLocation
      )
    )
    
    utils::unzip(
      zipfile = file.path(
        tempLocation,
        paste0(moduleName,'.zip')
      ), 
      exdir = file.path(
        shinyAppLocation,
        'modules',
        moduleName
      )
    )
    
    ParallelLogger::logInfo(paste0('Shiny module ', moduleName, ' downloaded from ', user, '/', repository))
    
  }
  
  return(
    file.path(
      shinyAppLocation,
      'modules'
    )
  )
  
}

createShinyFiles <- function(config, shinyAppLocation){
  # save text to UI.R
  writeLines(createUiText(config), file.path(shinyAppLocation, 'UI.R'))
  # save text to UI.R
  writeLines(createServerText(config), file.path(shinyAppLocation, 'server.R'))
}

createUiText <- function(config){
  
  # create the UI.R for the shiny app
  details <- data.frame(
    name = unlist(lapply(config$shinyModules, function(x) x$name)),
    order  = unlist(lapply(config$shinyModules, function(x) x$order)),
    infoBoxFile = unlist(lapply(config$shinyModules, function(x) x$infoBoxFile)),
    icon = unlist(lapply(config$shinyModules, function(x) x$icon))
  )
  
  
  # create the source text
  details <- details %>%
    dplyr::mutate(
      source = glue::glue('source("modules/{name}/module.R") \n'),
      tabs = glue::glue('shinydashboard::tabItem( \n
        tabName = "{name}", \n
        {name}Viewer({name}) \n
      )\n'
      )
    ) %>% 
    dplyr::arrange(.data$order)
  
  uiText <- paste0(details$source, collapse= '\n')
  
  
  # next start the UI
  uiText  <- paste(uiText, 
                   'ui <- shinydashboard::dashboardPage( \n
         skin = "black",  \n
  
  shinydashboard::dashboardHeader( \n
    title = "OHDSI Analysis Viewer", \n
    tags$li( \n
      shiny::div( \n
        shiny::img( \n
          src = "logo.png", \n
          title = "OHDSI", \n
          height = "40px", \n
          width = "40px" \n
          ), \n
        style = "padding-top:0px; padding-bottom:0px; \n"
      ),\n
      class = "dropdown" \n
    ) \n
  ), \n
  \n
  shinydashboard::dashboardSidebar( \n
    shinydashboard::sidebarMenuOutput("sidebarMenu") \n
  ), # end sidebar \n
  \n
    # ADD EACH MODULE SHINY AS A TAB ITEM \n
  shinydashboard::dashboardBody( \n
    shinydashboard::tabItems( \n
  ',
                   
                   sep = '\n', 
                   collapse = '\n'
  )
  uiText <- paste(
    uiText,  
    paste0(details$tabs, collapse =',\n'),
    ')',
    ')',
    ')', 
    sep = '\n', collapse= '\n'
  )
  
  return(uiText)
}

createServerText <- function(config){
  
  # create the UI.R for the shiny app
  details <- data.frame(
    name = unlist(lapply(config$shinyModules, function(x) x$name)),
    order  = unlist(lapply(config$shinyModules, function(x) x$order)),
    infoBoxFile = unlist(lapply(config$shinyModules, function(x) x$infoBoxFile)),
    icon = unlist(lapply(config$shinyModules, function(x) x$icon)),
    databaseConnectionKeyService = unlist(lapply(config$shinyModules, function(x) x$databaseConnectionKeyService)),
    databaseConnectionKeyUsername = unlist(lapply(config$shinyModules, function(x) x$databaseConnectionKeyUsername))
    
  )
  
  
  # create the source text
  details <- details %>%
    dplyr::mutate(
      addInfo = glue::glue('
      addInfo(
       item = shinydashboard::menuItem(
       text = "{name}", 
       tabName = "{name}", 
       icon = shiny::icon("{icon}")
       ), 
       infoId = "{name}Info"
      ) \n'),
      helper = glue::glue(
        'shiny::observeEvent(input$<<name>>Info, {
           showInfoBox("About", "www/<<infoBoxFile>>")
         })\n', .open = "<<", .close = ">>"  
      ),
      zeroValues = glue::glue('{name} = 0'),
      server = 
        
        dplyr::case_when(
          databaseConnectionKeyService != 'null' ~ 
            
            glue::glue(
              'if(input$menu == "<<name>>" & runServer[["<<name>>"]]==1){
          <<name>>Server(
            id = "<<name>>",
            resultDatabaseSettings = jsonlite::fromJSON(
             keyring::key_get(
              "<<databaseConnectionKeyService>>", 
              "<<databaseConnectionKeyUsername>>"
             )
            )
         )
        }', .open = "<<", .close = ">>"
              
            ),
          
          databaseConnectionKeyService == 'null' ~ 
            glue::glue(
              'if(input$menu == "<<name>>" & runServer[["<<name>>"]]==1){
          <<name>>Server(
            id = "<<name>>"
            )
        }', .open = "<<", .close = ">>"
            )
        )
      
    ) %>% 
    dplyr::arrange(.data$order)

  
  # create the server.R for the shiny app
  serverText <- paste('
  server <- shiny::shinyServer(function(input, output, session) { \n
  session$onSessionEnded(shiny::stopApp) \n
  \n
  #============= \n
  # sidebar menu \n
  #============= \n
  \n
  output$sidebarMenu <- shinydashboard::renderMenu( \n
    shinydashboard::sidebarMenu( \n
      id = "menu", \n
      ',
                      
                      paste0(details$addInfo, collapse= ', \n'),
                      sep = '\n'
  )
  
  serverText <- paste(
    serverText, 
    ') \n', 
    ') \n', 
    sep = '\n'
  )  
  
  serverText <- paste(
    serverText,
    '  
    #=============
    # Helper
    #=============
    ',
    paste0(details$helper, collapse = '\n'), 
    sep = '\n'
  )
  
  
  serverText<- paste0(
    serverText,
    '\n
  #=============
  # module severs
  #=============',
    '\n',
    'runServer <- shiny::reactiveValues( \n',
    
    paste0(details$zeroValues, collapse = ', \n'),
    '\n ) \n',
    
    'shiny::observeEvent(input$menu,{ \n
    runServer[[input$menu]] <- runServer[[input$menu]] +1 \n',
    
    paste0(details$server, collapse = '\n'),
    
    '
   }
  )
 }
)'
  )
  
  serverText <- paste0(
    serverText, 
    '
# helper \n

addInfo <- function(item, infoId) { \n
  infoTag <- tags$small( \n
    class = "badge pull-right action-button", \n
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;", \n
    type = "button",  \n
    id = infoId, \n
    "i" \n
  ) \n
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag)) \n
  return(item) \n
} \n
\n
showInfoBox <- function(title, htmlFileName) { \n
  shiny::showModal(shiny::modalDialog( \n
    title = title, \n
    easyClose = TRUE, \n
    footer = NULL, \n
    size = "l", \n
    shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) ) \n
  )) \n
} \n
  ', 
    sep = '\n'
  )
  
  return(serverText)
}
