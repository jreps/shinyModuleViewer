predictorViewer <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::p('Probast 2.2: Were predictor assessments made without knowledge of outcome data? (if outcome occur shortly after index this may be problematic)'),
    shiny::p(''),
    shiny::uiOutput(ns("predictorDropdown")),
  
    plotly::plotlyOutput(ns('predictorPlot'))
  )
}

predictorServer <- function(
  id, 
  summaryTable, 
  resultRow, 
  mySchema, 
  con,
  inputSingleView,
  myTableAppend,
  targetDialect
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(
        resultRow(),
        {
          if(!is.null(resultRow())){
      
      predTable <- getPredictors(
        con = con, 
        mySchema = mySchema, 
        targetDialect = targetDialect, 
        myTableAppend = myTableAppend,
        diagnosticId = summaryTable[resultRow(),'diagnosticId']
      )
      
      output$predictorDropdown <- shiny::renderUI({
        shiny::selectInput(
          label = 'Select Parameter',
          multiple = F, 
          inputId = session$ns('predictorParameters'),
          choices = unique(predTable$type),
          selected = unique(predTable$type)[1]
        )
      })
      
      
      tempPredTable <-  predTable %>% 
        dplyr::filter(
          .data$type == ifelse(
            is.null(input$predictorParameters), 
            unique(predTable$type)[1],
            input$predictorParameters
          )
        ) %>%
        dplyr::select(
          .data$daystoevent, 
          .data$outcomeattime, 
          .data$observedatstartofday
        ) %>%
        dplyr::mutate(
          survivalT = (.data$observedatstartofday-.data$outcomeattime)/.data$observedatstartofday
        ) %>%
        dplyr::filter(
          !is.na(.data$daystoevent)
        )

      tempPredTable$probSurvT  <- unlist(
          lapply(
            1:length(tempPredTable$daystoevent), 
            function(x){prod(tempPredTable$survivalT[tempPredTable$daystoevent <= tempPredTable$daystoevent[x]])}
          )
        )
      
      
      output$predictorPlot <- plotly::renderPlotly({
        plotly::plot_ly(x = ~ tempPredTable$daystoevent) %>% 
          plotly::add_lines(
          y = tempPredTable$probSurvT, 
          name = "hv", 
          line = list(shape = "hv")
          )
      })
      
    } # if null
  }) # observed event
      
        }
  )
}


getPredictors <- function(
  con, 
  mySchema, 
  targetDialect, 
  myTableAppend = '',
  diagnosticId = 1
){
  
  ParallelLogger::logInfo("gettingDb predictor diagnostics")
  
  sql <- "SELECT *
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_PREDICTORS
          where DIAGNOSTIC_ID = @diagnostic_id"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           my_table_append = myTableAppend,
                           diagnostic_id = diagnosticId)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  
  return(result)
}