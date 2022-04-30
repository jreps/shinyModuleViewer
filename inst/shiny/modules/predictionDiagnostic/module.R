# @file module.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

predictionDiagnosticViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
 moduleFiles <- dir(file.path("modules",strsplit(id,'-')[[1]][1],"modules"), pattern = '.R')
  if(length(moduleFiles)>0){
    for(fileLoc in moduleFiles){
      source(
        file.path(
        "modules",
        strsplit(id,'-')[[1]][1],
        "modules", 
        fileLoc
        ), 
        local=TRUE
        )
    }
  }
  
  shiny::tabsetPanel(
    id = ns('allView'),
    shiny::tabPanel(
      "All Diagnostic Summary",  
      summaryDiagnosticViewer(id = ns('sumTab'))
    ),
    
    shiny::tabPanel(
      "Explore Selected Diagnostics",
      
      shiny::tabsetPanel(
        id = ns('singleView'),
        shiny::tabPanel(
          "Participants",
          participantViewer(ns('participants'))
        ),
        
        shiny::tabPanel(
          "Predictors",
          predictorViewer(ns('predictors'))
        ),
        
        shiny::tabPanel(
          "Outcomes", 
          outcomeViewer(ns('outcomes'))
        )
        
      )
    )
    
  )
  
}

predictionDiagnosticServer <- function(
  id, 
  resultDatabaseSettings = list(myPort = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session, diagnosticDatabaseSettings = resultDatabaseSettings) {
      
      moduleFiles <- dir(file.path("modules",strsplit(id,'-')[[1]][1],"modules"), pattern = '.R')
      if(length(moduleFiles)>0){
        for(fileLoc in moduleFiles){
          source(
            file.path(
              "modules",
              strsplit(id,'-')[[1]][1],
              "modules", 
              fileLoc
            ), 
            local=TRUE
          )
        }
      }
      
      # connect
      if(F){
      if(diagnosticDatabaseSettings$myPort != ""){
        ParallelLogger::logInfo('Port')
        ParallelLogger::logInfo(paste(diagnosticDatabaseSettings$myPort))
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = diagnosticDatabaseSettings$targetDialect,
                            server = diagnosticDatabaseSettings$myServer,
                            user = diagnosticDatabaseSettings$myUser,
                            password = diagnosticDatabaseSettings$myPassword,
                            port = diagnosticDatabaseSettings$myPort)
        
      } else{
        ParallelLogger::logInfo('No Port')
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = diagnosticDatabaseSettings$targetDialect,
                            server = diagnosticDatabaseSettings$myServer,
                            user = diagnosticDatabaseSettings$myUser,
                            password = diagnosticDatabaseSettings$myPassword
                            )
        
      }
      
      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          pool::poolClose(con)
        }
      })
      }
      
      # old connection 
      
      conDetails <- DatabaseConnector::createConnectionDetails(
        dbms = diagnosticDatabaseSettings$targetDialect, 
        server = diagnosticDatabaseSettings$server
        )
      con <- DatabaseConnector::connect(connectionDetails = conDetails)

      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          DatabaseConnector::disconnect(con)
        }
      })
      
      
      
      
      # get data
      getDbSummary <- function(
        con, 
        mySchema, 
        targetDialect, 
        myTableAppend = '',
        threshold1_2 = 0.95
      ){
        ParallelLogger::logInfo("gettingDb summary")
        
        sql <- "SELECT distinct design.DIAGNOSTIC_ID,
          design.DATABASE,
          design.TARGET_JSON,
          design.OUTCOME_JSON,
          probast.PROBAST_ID,
          probast.RESULT
          
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_DESIGN_SETTINGS design inner join
          @my_schema.@my_table_appendDIAGNOSTIC_PROBAST probast
          on design.DIAGNOSTIC_ID = probast.DIAGNOSTIC_ID"
        
        sql <- SqlRender::render(sql = sql, 
                                 my_schema = mySchema,
                                 my_table_append = myTableAppend)
        
        sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
        
        summaryTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
        colnames(summaryTable) <- SqlRender::snakeCaseToCamelCase(colnames(summaryTable))
        
        summaryTable$targetName <- unlist(
          lapply(
            summaryTable$targetJson, 
            function(x){jsonlite::unserializeJSON(x)$name}
          )
        )
        summaryTable$outcomeName <- unlist(
          lapply(
            summaryTable$outcomeJson, 
            function(x){jsonlite::unserializeJSON(x)$name}
          )
        )
        
        summary <- summaryTable %>% tidyr::pivot_wider(
          id_cols = c(
            'diagnosticId', 
            'database', 
            'targetName', 
            'outcomeName'
          ),
          names_from = 'probastId',
          values_from = 'result'
        ) %>% 
          dplyr::mutate(
            `1.2` = ifelse(
              .data$`1.2.1`>=threshold1_2 & 
                .data$`1.2.2`>=threshold1_2 &
                .data$`1.2.3`>=threshold1_2 & 
                .data$`1.2.4`>=threshold1_2,
              'Pass', 
              'Fail'
            )
          ) %>%
          dplyr::select(
            -c(
              '1.2.1', 
              '1.2.2', 
              '1.2.3', 
              '1.2.4'
            )
          ) %>%
          dplyr::relocate(.data$`1.2`, .after = .data$`1.1`)
        ParallelLogger::logInfo("got summary")
        return(summary)
      }
      summaryTable <- getDbSummary(
        con = con, 
        mySchema = diagnosticDatabaseSettings$mySchema, 
        targetDialect = diagnosticDatabaseSettings$targetDialect,
        myTableAppend = diagnosticDatabaseSettings$myTableAppend#,threshold1_2
      )


      # use the summary module to select a result via row selection
      resultRow <- summaryDiagnosticServer(id = 'sumTab', summaryTable)
      
      # change to single model explore tab when summary table row is selected
      shiny::observeEvent(resultRow(), {
        shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Diagnostics")
      })
      
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      

        participantServer(
          id = 'participants',
          summaryTable, 
          resultRow, 
          mySchema = diagnosticDatabaseSettings$mySchema, 
          con,
          inputSingleView = input$singleView,
          myTableAppend = diagnosticDatabaseSettings$myTableAppend, 
          targetDialect = diagnosticDatabaseSettings$targetDialect
        ) 
      
      predictorServer(
        id = 'predictors',
        summaryTable, 
        resultRow, 
        mySchema = diagnosticDatabaseSettings$mySchema, 
        con,
        inputSingleView = input$singleView,
        myTableAppend = diagnosticDatabaseSettings$myTableAppend, 
        targetDialect = diagnosticDatabaseSettings$targetDialect
      ) 
      
      outcomeServer(
        id = 'outcomes',
        summaryTable = summaryTable, 
        resultRow = resultRow, 
        mySchema = diagnosticDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = input$singleView,
        myTableAppend = diagnosticDatabaseSettings$myTableAppend, 
        targetDialect = diagnosticDatabaseSettings$targetDialect
      ) 
      
      
      
      
    }
  )
}



