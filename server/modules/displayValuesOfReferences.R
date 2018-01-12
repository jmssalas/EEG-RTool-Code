library(shiny)

displayValuesOfReferencesUI <- function(id)
{
  ns <- NS(id)
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h5(strong('Baselines:')),
      p('Click a row for select a baseline task and display its values:'),
      DT::dataTableOutput(ns('baselines'))
    ),
    hr(),
    uiOutput(ns('valOfReferencesUI'))
  )
}

displayValuesOfReferences <- function(input, output, session)
{
  ns <- session$ns
  
  getBaselines <- reactive({
    description  <- getDataDescription(dataSelected)
    baselines    <- getValuesOfReferences(dataSelected)
    numBaselines <- length(names(baselines))
    numCols      <- length(colnames(description))

    m <- data.frame()
    for (i in 1:numBaselines)
    {
      value <- description[description$startMark == baselines[[i]]$startMark, ]
      for (j in 1:numCols)
      {
        m[i,j] <- value[1,j]
      }
    }
     
    colnames(m) <- c('Task Name', 'Start Time', 'End Time', 'Start Mark', 'End Mark')
    
    return(m)
  })
  
  
  # Render Data table of all baselines
  output$baselines = DT::renderDataTable(
    expr = getBaselines(), server = TRUE, selection = 'single',
    options = list(columnDefs = list(list(className = 'dt-center')),
                   pageLength = 5)
  )
  
  # Get values for dataTable
  getValues <- function(baseline){
    # Get values of reference
    values <- getValuesOfReferences(dataSelected)[[baseline]]$values
    
    # Create matrix 2x4
    m <- matrix(ncol=4, nrow=2)
    colnames(m) <- c('Attention Index', 'Memorization Index', 
                     'Approach Widthdrawal Index', 'Engagement Index')
    rownames(m) <- c('mean', 'std')
    
    # Fill the matrix
    for (value in rownames(m))
    {
      for (i in 1:length(values))
      {
        m[value,i] <- values[[i]][[value]]
      } 
    }
    # Return matrix like data.frame
    return(as.data.frame(m))
  }
  
  # ObserveEvent for selected rows
  observeEvent(input$baselines_rows_selected, {
    row <- input$baselines_rows_selected
    
    if (is.null(row)) return()
    
    baselines <- getBaselines()[row,]
    selectedBasal <- paste('basal', baselines$`Start Mark`, sep='-')
    
    # RenderUI for values of referecne 
    output$valOfReferencesUI <- renderUI({
      ui <- fluidRow(
        h5(strong('Values of reference:')),
        DT::dataTableOutput(ns('valuesOfReferences')),
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
    output$valuesOfReferences <- DT::renderDataTable(expr = getValues(selectedBasal),
                                        options = list(paging = FALSE, searching = FALSE))
  })
  
  
  valuesOfReferencePlots <- reactive({
    # Check if 'input$indicesSelect' is null
    if (isNull(input$indicesSelect)) return() # If indices have not been selected, show only graphics.
    
    # Check selected row
    row <- input$baselines_rows_selected
    if (is.null(row)) return()
    
    baselines <- getBaselines()[row,]
    selectedBasal <- paste('basal', baselines$`Start Mark`, sep='-')
    
    # Get selected indices
    indicesSelected <- strsplit(input$indicesSelect, ' ')
    # Get values of reference
    values <- getValuesOfReferences(dataSelected)[[selectedBasal]]$values
    
    plot_output_list <- lapply(indicesSelected, function(indice) {
      # Get indice point2point
      p2p <- values[[indice]]$p2p
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
  
  return(valuesOfReferencePlots)
}