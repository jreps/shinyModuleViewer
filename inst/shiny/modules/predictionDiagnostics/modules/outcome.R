outcomeViewer <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::p('Probast 3.1: Was the outcome determined appropriately? (Are age/sex/year/month trends expected?)'),
    shiny::p(''),
    shiny::uiOutput(ns("outcomeDropdown")),
    
    plotly::plotlyOutput(ns('outcomePlot'))
  )
}

outcomeServer <- function(
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
            
            
      outcomeTable <- getOutcomes(
        con = con, 
        mySchema = mySchema, 
        targetDialect = targetDialect, 
        myTableAppend = myTableAppend,
        diagnosticId = summaryTable[resultRow(),'diagnosticId']
      )
      
      output$outcomeDropdown <- shiny::renderUI({
        shiny::selectInput(
          label = 'Select Parameter',
          multiple = F, 
          inputId = session$ns('outcomeParameters'),
          choices = unique(outcomeTable$aggregation),
          selected = unique(outcomeTable$aggregation)[1]
        )
      })
      
      #plot: xvalue, outcomepercent, group by type -- filter: aggregation
      output$outcomePlot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = outcomeTable %>%
            dplyr::filter(
              .data$aggregation == ifelse(
                is.null(input$outcomeParameters),
                unique(outcomeTable$aggregation)[1],
                input$outcomeParameters
              )
            ), 
          x = ~xvalue, 
          y = ~outcomepercent, 
          group = ~type,
          color = ~type,
          type = 'scatter', 
          mode = 'lines'
        ) %>%
          plotly::layout(
            title = "Outcome rate",
            xaxis = list(title = "Value"),
            yaxis = list (title = "Percent of cohort with outcome")
            )
      })
        
            
      
 
          }
          }
      )
      
    }
  )
}

getOutcomes <- function(
  con, 
  mySchema, 
  targetDialect, 
  myTableAppend = '',
  diagnosticId = 1
){
  
  ParallelLogger::logInfo("gettingDb outcome diagnostics")
  
  sql <- "SELECT *
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_OUTCOME
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