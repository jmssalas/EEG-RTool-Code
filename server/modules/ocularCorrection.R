library(shiny)

ocularCorrectionUI <- function(id) 
{
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             shiny::dataTableOutput(ns('eegTable'))
      ),
      column(6,
             shiny::dataTableOutput(ns('eogTable'))
      )
    ),
    
    br(), hr(),
    h5(strong('Press button to apply ocular correction')),
    actionButton(ns('applyGratton'), 'Apply Ocular Correction'),

    hr(),
    uiOutput(ns('selectCorrectedEEGUI'))
  )
}

ocularCorrection <- function(input, output, session)
{
  ns <- session$ns
  
  getFilteredEEGSignals <- reactive({ 
    eeg <- getEEGSignals(dataSelected)
    if (is.null(eeg)) return (data.frame())
    df <- data.frame(eeg)
    colnames(df) <- c('EEG Signals')
    return(df)
  })
  
  getFilteredEOGSignals <- reactive({ 
    eog <- getEOGSignals(dataSelected)
    if (is.null(eog)) return (data.frame())
    df <- data.frame(eog)
    colnames(df) <- c('EOG Signals')
    return(df)
  })
  
  output$eegTable = shiny::renderDataTable(
    expr = getFilteredEEGSignals(), options = list(paging = FALSE, searching = FALSE)
  )
  
  output$eogTable = shiny::renderDataTable(
    expr = getFilteredEOGSignals(), options = list(paging = FALSE, searching = FALSE)
  )
  
  # Function which get checkboxGroupInput of corrected EEG
  getCorrectedEEGUI <- function() 
  {
    ui <- fluidRow(
      column(12,
        checkboxGroupInput(ns('displayCorrectedEEG'), 'Corrected EEG:',
                           choices = names(getCorrectedEEG(dataSelected)), inline = TRUE),
        hr()
      )
    )
    return(ui)
  }
  
  observeEvent(input$applyGratton, {
    eegNames <- getEEGSignals(dataSelected)
    eogNames <- getEOGSignals(dataSelected)
    
    eeg <- list()
    # Join all EEG Signals
    for (name in eegNames)
    {
      eeg[[name]] <- getDataFilteredSignal(dataSelected, name)
    }
    
    if (length(eogNames) != 1) 
    {
      showModal(modalDialog(
        title = 'Error!', 'There can only be one EOG signal.'
      ))
      return()
    }
    eog <- getDataFilteredSignal(dataSelected, eogNames)
    
    eegMatrix <- convertListToMatrix(eeg)
    
    # Show filtering display
    showModal(modalDialog(
      title = 'Please wait!','Applying ocular correction...', footer = NULL
    )) 
    
    result <- gratton(eegMatrix, eog)
    
    # Check if 'gratton' has had any problem
    if (!is.list(result)) 
    {
      showModal(modalDialog(
        title = 'Error!', result, footer = modalButton('Close')
      ))
      
      return()
    }
    
    # If 'gratton' has not had any problem, then, store result (converted in list)
    addCorrectedEEGToUserData(dataSelected = dataSelected, data = convertMatrixToList(result$ceeg))
    addCorrectedEOGToUserData(dataSelected = dataSelected, data = result$neog)
    
    # Show filter finish display
    showModal(modalDialog(
      title = 'Perfect!', 'Ocular correction has been applied', footer = modalButton('Close')
    ))
    
    # When EEG has been corrected, show the checkbox to can show it
    output$selectCorrectedEEGUI <- renderUI({ getCorrectedEEGUI() })
  })
  
  
  # Get plot of signal 'signal'
  getPlotOfSignal <- function(signal) 
  {
    # Get signal Data
    signalData <- getCorrectedEEG(dataSelected)[[signal]]
    # Create signal list with needed fields to dygraph
    signalList <- list('Time' = 1:length(signalData), 'Normal' = signalData)
    
    # Create dygraph
    plot <- dygraph(data = signalList, main = signal) %>%
      dyUnzoom() %>%
      dyRangeSelector() %>%
      dyAxis('x', drawGrid = FALSE, label = 'Time (mseg)') %>%
      dyAxis('y', label = 'Electric Potential (microV)')
    
    # Return dygraph
    return(plot)
  }
  
  
  # Get selected signals of 'input$displayCorrectedEEG'
  getSelectedSignals <- reactive({
    # Check if 'input$displayCorrectedEEG' is null
    if (isNull(input$displayCorrectedEEG)) return()
    
    selectedSignals <- strsplit(input$displayCorrectedEEG, ' ')
    return(selectedSignals)
  })
  
  # Reactive which contains the signal's plots for return to main server
  correctedEEGPlots <- reactive({
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
  return(correctedEEGPlots)
  
}