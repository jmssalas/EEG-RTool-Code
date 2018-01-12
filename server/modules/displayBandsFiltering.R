library(shiny)

displayBandsFilteringUI <- function(id)
{
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h5(strong('Data Tasks')),
      p('Click rows to select task and display its signal:'),
      DT::dataTableOutput(ns('tasks'))
    ),
    hr(),
    fluidRow(
      # Checkbox to select bands to show
      checkboxGroupInput(ns('bandsSelect'), label = 'Select bands which you want to display', 
                         choices = c('Normal signal' = 'signal',
                                     'Theta band' = 'theta',
                                     'Alpha band' = 'alpha',
                                     'Beta band' = 'beta'), 
                         inline = TRUE,
                         selected = 'signal'),
      hr()
    )
  )
}

displayBandsFiltering <- function(input, output, session)
{
  getTaskName <- function(task)
  {
    if (regexpr('<', task) > 0)
    {
      task <- strsplit(as.character(task), '<')
      task <- task[[1]][2]
      if (regexpr('>', task) > 0)
      {
        task <- strsplit(as.character(task), '>')
      }
    }
    return(task)
  }
  
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
  
  bandsFilteringPlots <- reactive({
    row <- input$tasks_rows_selected
    
    # Check if selected row is null
    if (is.null(row)) return()
    # Check if 'input$bandsSelect' is null
    if (isNull(input$bandsSelect)) 
      # If bands have not been selected, show only graphics.
    { bandsSelected <- list() } 
    else 
      # If bans have been selected, get selected bands to show with normal signal.
    { bandsSelected <- strsplit(input$bandsSelect, ' ') }
    
    # Get selected row
    rowSelected <- getDataDescription(dataSelected)[row,]
    # Get selected task
    task <- rowSelected[[1]]
    
    # Get signals segmentation
    signalsSegmentation <- getSignalsSegmentation(dataSelected)
    
    # Get signals of selected task
    taskSignals <- list()
    signalList <- list()
    for (signal in names(signalsSegmentation))
    {
      taskSignals <- getTaskSignals(dataSelected, signal, task, row)

      signalList[[signal]] <- list('Time' = 1:length(taskSignals[[signal]]$signal))
      for (band in bandsSelected)
      {
        signalList[[signal]][[band]] <- taskSignals[[signal]][[band]]
      }
    }
    
    ui <- fluidRow(
      column(6,
             renderDygraph({ 
               # Create dygraph for sensor A
               plot <- dygraph(data = signalList[['Sensor-A:EEG']], main = paste(getTaskName(task),'Sensor-A:EEG',sep=' - ')) %>%
                 dyUnzoom() %>%
                 dyRangeSelector() %>%
                 dyAxis('x', drawGrid = FALSE, label = 'Time (mseg)') %>%
                 dyAxis('y', label = 'Electric Potential (microV)') %>%
                 dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))
               return(plot)
             })
      ),
      column(6,
             renderDygraph({ 
               # Create dygraph for sensor B
               plot <- dygraph(data = signalList[['Sensor-B:EEG']], main = paste(getTaskName(task),'Sensor-B:EEG',sep=' - ')) %>%
                 dyUnzoom() %>%
                 dyRangeSelector() %>%
                 dyAxis('x', drawGrid = FALSE, label = 'Time (mseg)') %>%
                 dyAxis('y', label = 'Electric Potential (microV)') %>%
                 dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))
               
               return(plot)
             })
      ),
      hr()
    )
    return (ui)
  })
  
  return(bandsFilteringPlots)
}