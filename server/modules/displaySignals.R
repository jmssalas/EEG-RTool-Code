library(shiny)
library(dygraphs)

# This function has been created for check if 'values' is null because
# when the checkboxgroup's all values are unchecked, the return value is
# NULL but is.null function cannot detect it directly. For this, this
# function is necesary, because like param is.null function can detect it.
isNull <- function(values)
{
  return(is.null(values))
}


displaySignalsUI <- function(id)
{
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h5(strong('Data Tasks')),
      p('Click rows to select task:'),
      DT::dataTableOutput(ns('tasks'))
    ),
    
    hr(),
    fluidRow(
      h5(strong('Data Signals')),
      p('Select signals to show:'),
      
      uiOutput(ns('uiSelectSignals')),
      hr()
    )
  )
}

displaySignals <- function(input, output, session) 
{
  ns <- session$ns
  
  # RenderUI for checkboxGroup where select the signals to show
  output$uiSelectSignals <- renderUI({
    # Checkbox to select signals to show
    checkboxGroupInput(ns('selectSignals'), 'Signals to show:',
                       choices = names(getDataSignals(dataSelected)),
                       inline = TRUE)
  })

  # Get tasks for dataTable
  getTasks <- reactive({
    description <- getDataDescription(dataSelected)
    colnames(description) <- c('Task Name', 'Start Time', 'End Time', 'Start Mark', 'End Mark')
    return(description)
  })
  
  # Render Data table of Tasks
  output$tasks = DT::renderDataTable(
    expr = getTasks(), server = TRUE, selection = 'single',
    options = list(columnDefs = list(list(className = 'dt-center')),
                   pageLength = 5)
  )
  
  # Reactive for select one task
  dateWindow <- reactive({
    row <- input$tasks_rows_selected
    
    if (is.null(row)) return()
    
    # Get selected row
    rowSelected <- getTasks()[row,]
    
    # Get startMark and endMark columns
    startMark <- rowSelected[4]
    endMark <- rowSelected[5]
    
    # Get values
    startMark <- startMark[[1]]
    endMark <- endMark[[1]]
    
    return(c(startMark, endMark))
  })
  
  # Get plot of signal 'signal'
  getPlotOfSignal <- function(signal) 
  {
    # Get signal Data
    signalData <- getDataSignal(dataSelected, signal)
    # Create signal list with needed fields to dygraph
    signalList <- list('Time' = 1:length(signalData), 'Normal' = signalData)
    
    # Check if 'signal' has been filtered to display too.
    if (signalHasBeenFiltered(dataSelected, signal))
    {
      filteredData <- getDataFilteredSignal(dataSelected, signal)
      signalList[['Filtered']] <- filteredData
    }
    
    # Create dygraph
    plot <- dygraph(data = signalList, main = signal) %>%
      dyUnzoom() %>%
      dyRangeSelector(dateWindow = dateWindow()) %>%
      dyAxis('x', drawGrid = FALSE, label = 'Time (mseg)') %>%
      dyAxis('y', label = 'Electric Potential (microV)')
    
    # Return dygraph
    return(plot)
  }
  
  # Get selected signals of 'input$selectSignals'
  getSelectedSignals <- reactive({
    # Check if 'input$selectSignals' is null
    if (isNull(input$selectSignals)) return()
    
    selectedSignals <- strsplit(input$selectSignals, ' ')
    return(selectedSignals)
  })
  
  # Reactive which contains the signal's plots for return to main server
  signalsPlots <- reactive({
    selectedSignals <- getSelectedSignals()
    plot_output_list <- lapply(selectedSignals, function(signal) {
      fluidRow(
        renderDygraph({ getPlotOfSignal(signal) }),
        hr()
      )
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
    
    return (plot_output_list)
  })
  
  # Return set signal's plots
  return(signalsPlots)
}
