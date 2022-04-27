summaryDiagnosticViewer <- function(id){
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns('summaryTable'))
}

summaryDiagnosticServer <- function(
  id, 
  summaryTable
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # check if this makes drpdwn filter
      summaryTable$targetName <- as.factor(summaryTable$targetName)
      summaryTable$outcomeName <- as.factor(summaryTable$outcomeName)
      
      dataTab <- DT::datatable(
          summaryTable %>% dplyr::select(-c('diagnosticId')),
          rownames= FALSE, 
          selection = 'single', 
          filter = 'top',
          extensions = 'Buttons', 
          options = list(
            dom = 'Blfrtip' , 
            buttons = c(I('colvis'), 'copy', 'excel', 'pdf' ),
            scrollX = TRUE
            #pageLength = 100, lengthMenu=c(10, 50, 100,200)
          ),
          
          container = htmltools::withTags(
            table(
              class = 'display', 
              thead(
                #tags$th(title=active_columns[i], colnames(data)[i])
                tr(apply(
                  data.frame(
                    colnames = c(
                      'Database', 
                      'targetName',
                      'outcomeName',
                      '1.1',
                      '1.2',
                      '2.1',
                      '2.2',
                      '2.3',
                      '3.4',
                      '3.6',
                      '4.1'
                    ), 
                    labels = c('Database used to diagnose model design', 
                               'Target population - the patients you want to predict risk for',
                               'Outcome - what you want to predict',
                               '1.1 : Were appropriate data sources used, e.g. cohort, RCT or nested case-control study data?',
                               '1.2 : Were all inclusions and exclusions of participants appropriate?',
                               '2.1 : Were predictors defined and assessed in a similar way for all participants?',
                               '2.2 : Were predictor assessments made without knowledge of outcome data?',
                               '2.3 : Are all predictors available at the time the model is intended to be used?',
                               '3.4 : Was the outcome defined and determined in a similar way for all participants?',
                               '3.6 : Was the time interval between predictor assessment and outcome determination appropriate?',
                               '4.1 : Were there a reasonable number of participants with the outcome?'
                    )
                  ), 1,
                  function(x) th(title=x[2], x[1])
                )
                )
              )
            )
          )
          
        )
        
        for(probastName in colnames(summaryTable)[!colnames(summaryTable) %in% c('diagnosticId', 'targetName', 'outcomeName', 'Database')]){
          dataTab  <- dataTab  %>% 
            DT::formatStyle(
              columns = probastName,
              valueColumns = probastName,
              backgroundColor = DT::styleEqual(
                levels = c('Pass','Unknown','Fail'), 
                values = c("#DAF7A6","#FFC300","#FF5733")
              )
            )
        }
        
      output$summaryTable <- DT::renderDataTable(dataTab)
      
      selectedRow <- shiny::reactive({
        input$summaryTable_rows_selected
      })
      
      return(selectedRow)
      
    }
  )
}

