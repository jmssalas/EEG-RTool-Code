library(shiny)

displayIndicesUI <- function(id)
{
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h5(strong('Data Tasks')),
      p('Click rows to select task and display its indices:'),
      DT::dataTableOutput(ns('tasks'))
    ),
    hr(),
    uiOutput(ns('indicesValues'))
  )
}

displayIndices <- function(input, output, session)
{
  ns <- session$ns
  # Get tasks for dataTable
  getTasks <- reactive({
    description  <- getDataDescription(dataSelected)
    indices      <- getIndices(dataSelected)
    tasksNames   <- names(indices)
    numTasks     <- length(tasksNames)
    numCols      <- length(colnames(description))
    
    m <- data.frame()
    for (i in 1:numTasks)
    {
      value <- description[description$taskName == tasksNames[i], ]
      for (j in 1:numCols)
      {
        m[i,j] <- value[1,j]
      }
    }
    colnames(m) <- c('Task Name', 'Start Time', 'End Time', 'Start Mark', 'End Mark')
    return(m)
  })
  
  # Render Data table of Tasks
  output$tasks = DT::renderDataTable(
    expr = getTasks(), server = TRUE, selection = 'single',
    options = list(columnDefs = list(list(className = 'dt-center')),
                   pageLength = 5)
  )
  
  # Get values for dataTable
  getValuesOfIndices <- function(task){
    # Get indices of task
    indices <- getIndices(dataSelected)[[task]]
    # Create matrix 1x4
    m <- matrix(nrow = 1, ncol = 4)
    colnames(m) <- c('Attention Index', 'Memorization Index', 
                     'Approach Widthdrawal Index', 'Engagement Index')
    rownames(m) <- 'value'
    
    # Fill matrix
    for (index in 1:length(indices))
    {
      m[1,index] <- indices[[index]]$value
    }
    # Return matrix like data.frame
    return(as.data.frame(m))
  }
  
  # ObserveEvent for selected rows
  observeEvent(input$tasks_rows_selected, {
    row <- input$tasks_rows_selected
    # Check if row is null -> return()
    if (is.null(row)) return()
    
    # Get task of selected row
    task <- getTasks()[row, 1]

    # RenderUI for task indices 
    output$indicesValues <- renderUI({
      ui <- fluidRow(
        h5(strong('Indices values')),
        shiny::dataTableOutput(ns('indices')),
        hr(),
        checkboxGroupInput(ns('indicesSelect'), label = 'Select indices which you want to display', 
                           choices = c('Attention Index' = 'AI', 
                                       'Memorization Index' = 'MI', 
                                       'Approach Widthdrawal Index' = 'AW', 
                                       'Engagement Index' = 'EI'), 
                           inline = TRUE,
                           selected = 'signal')
         
      )
      return(ui)
    })
    
    # Visualizate dataTAble for values of indices
    output$indices <- shiny::renderDataTable(expr = getValuesOfIndices(task),
                                             options = list(paging = FALSE, searching = FALSE))
  })
  
  # Reactive for getting plots
  indicesPlots <- reactive({
    # Check if 'input$indicesSelect' is null
    if (isNull(input$indicesSelect)) return() # If indices have not been selected, show only graphics.
    # If bans have been selected, get selected bands to show with normal signal.
    
    # Get selected indices
    indicesSelected <- strsplit(input$indicesSelect, ' ')
    # Get values of reference
    values <- getIndices(dataSelected)
    
    row <- input$tasks_rows_selected
    if (is.null(row)) return()
    task <- getTasks()[row, 1]
    
    plot_output_list <- lapply(indicesSelected, function(indice) {
      # Get indice point2point
      p2p <- values[[task]][[indice]]$ZSp2p
      # Create signalList
      signalList <- list('Time' = 1:length(p2p), 'Value' = p2p)
      fluidRow(
        renderDygraph({
          # Create dygraph 
          plot <- dygraph(data = signalList, main = indice) %>%
            dyUnzoom() %>%
            dyRangeSelector() %>%
            dyAxis('x', drawGrid = FALSE, label = 'Time (mseg)') 
          return(plot)
        }),
        hr()
      )
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
    
    return (plot_output_list)
  })
  
  return(indicesPlots)
  
}