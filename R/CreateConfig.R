#' createModuleConfig
#'
#' @description
#' Create an R list with the config specification
#'
#' @details
#' User specifies the settings to create a config for a module
#' 
#' @param configList An R list with the config settings
#' @param configLocation  The location to save the config json file                                     
#' @return
#' An R list with the module config settings
#'
#' @export
createModuleConfig <- function(
  connectionDetails,
 moduleId = 'about',
 tabName = "About",
 shinyModulePackage = 'OhdsiShinyModules',
 moduleUiFunction = "aboutViewer",
 moduleServerFunction = "aboutServer",
 moduleDatabaseConnectionKeyService = NULL,
 moduleDatabaseConnectionKeyUsername = NULL,
 moduleInfoBoxFile =  "aboutHelperFile()",
 moduleIcon = "info",
 resultDatabaseDetails
){

  result <- list(
    id = moduleId,
    tabName = tabName,
    tabText = tabName,
    shinyModulePackage = shinyModulePackage,
    uiFunction = moduleUiFunction,
    serverFunction = moduleServerFunction,
    databaseConnectionKeyService = moduleDatabaseConnectionKeyService,
    databaseConnectionKeyUsername = moduleDatabaseConnectionKeyUsername,
    infoBoxFile = moduleInfoBoxFile,
    icon = moduleIcon
  )
  
  # setup key
  keyring::key_set_with_value(
    service = moduleDatabaseConnectionKeyService, 
    username = moduleDatabaseConnectionKeyUsername, 
    password = as.character(
      jsonlite::toJSON(
        resultDatabaseDetails
      ))
      )
  
  return(result)
}

createDefaultAboutConfig <- function(
  resultDatabaseDetails
){
  result <- list(
    id = 'about',
    tabName = 'About',
    tabText = 'About',
    shinyModulePackage = "OhdsiShinyModules",
    uiFunction = "aboutViewer",
    serverFunction = "aboutServer",
    databaseConnectionKeyService = NULL,
    databaseConnectionKeyUsername = NULL,
    infoBoxFile = "aboutHelperFile()",
    icon = "info"
  )
  
  # setup key
  keyring::key_set_with_value(
    service = 'databaseSettings', 
    username = 'about', 
    password = as.character(jsonlite::toJSON(
      resultDatabaseDetails
    ))
  )
}

createDefaultPredictionConfig <- function(
  resultDatabaseDetails
  ){
  
  result <- list(
    id = 'prediction',
    tabName = 'Prediction',
    tabText = 'Prediction',
    shinyModulePackage = "OhdsiShinyModules",
    uiFunction = "predictionViewer",
    serverFunction = "predictionServer",
    databaseConnectionKeyService = "DatabaseSettings",
    databaseConnectionKeyUsername = "prediction",
    infoBoxFile = "predictionHelperFile()",
    icon = "table"
  )
  
  # set up the service/username
  keyring::key_set_with_value(
    service = "databaseSettings", 
    username = "prediction", 
    password = as.character(jsonlite::toJSON(
      resultDatabaseDetails
    ))
  )
  
}