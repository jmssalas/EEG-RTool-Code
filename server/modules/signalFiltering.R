library(shiny)
library(dygraphs)
library(seewave)

signalFilteringUI <- function(id) 
{
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(3,
             uiOutput(ns('uiFiltSignal'))
      ),
      column(2,
             numericInput(ns('lowBand'), 'Low cut-off:', 1, min = 1, max = 100)
      ),
      column(2,
             numericInput(ns('highBand'), 'High cut-off:', 1, min = 1, max = 100)
      )
    ),
    
    h5(strong('Press button to filter signal')),
    actionButton(ns('filterSignalButton'), 'Filter signal'),
    
    hr(),
    uiOutput(ns('selFiltSignalUI'))
  )
}

signalFiltering <- function(input, output, session)
{
  ns <- session$ns

  # RenderUI for selectizeInput where select the signals to filter
  output$uiFiltSignal <- renderUI({
    # I() indicates it is raw JavaScript code that should be evaluated, instead
    # of a normal character string
    selectizeInput(ns('filtSignal'), 'Signal to filter', 
                   choices = getSignalsToFilter(dataSelected),
                   options = list(
                     placeholder = 'Please select a signal below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  
  # Function which get checkboxGroupInput of filtered signals
  getSelFiltSignalUI <- function() 
  {
    ui <- fluidRow(
      checkboxGroupInput(ns('selectFiltSignals'), 'Filtered signals to show:',
                         choices = names(getDataFilteredSignals(dataSelected)), inline = TRUE),
      hr()
    )
    return(ui)
  }
  
  # When signal has been filtered, show all filtered signals to can show it
  output$selFiltSignalUI <- renderUI({ getSelFiltSignalUI() })
  
  # ObserveEvent for input$lowBand to change highBand's min value to lowBand value. Also,
  # if lowBand is bigger than highBand, highBand's value is changed too.
  observeEvent(input$lowBand, {
    if (input$lowBand > input$highBand)
      updateNumericInput(session, 'highBand', value = input$lowBand)
    
    updateNumericInput(session, 'highBand', min = input$lowBand)
  })
  
  # ObserveEvent for 'input$filtSignal' to update numeric inputs 'lowBand' and 'highBand' values
  # depending on 'filtSignal'
  observeEvent(input$filtSignal, {
    if (input$filtSignal == 'Sensor-A:EEG' || input$filtSignal == 'Sensor-B:EEG')
    {
      updateNumericInput(session, 'lowBand', value = 2)
      updateNumericInput(session, 'highBand', value = 30)
    }
    else if (input$filtSignal == 'Sensor-D:EOG')
    {
      updateNumericInput(session, 'lowBand', value = 0.1)
      updateNumericInput(session, 'highBand', value = 5)
    }
  })
  
  # ObserveEvent for input$filterSignalButton to filter selected signal. Also, after filtering,
  # show all filtered signals to can select it and can show it like Display Signal Panel
  observeEvent(input$filterSignalButton, {
    if (input$filtSignal == '') 
    {
      showModal(modalDialog(
        title = 'Error!', 'You must select signal to filter!', footer = modalButton('Close')
      ))
      
      return()
    }
    
    # Get necesited values for filtering
    signal <- getDataSignal(dataSelected, input$filtSignal)
    srate <- getDataHeader(dataSelected)$outputRate
    locutoff <- input$lowBand
    hicutoff <- input$highBand
    
    # Show filtering display
    showModal(modalDialog(
      title = 'Please wait!','Filtering signal...', footer = NULL
    ))
    
    # Filter signal
    result <- eegfilt(data = signal, srate = srate, 
                      locutoff = locutoff, hicutoff = hicutoff)
    
    # Check if 'eegfilt' has had any problem
    if (!is.list(result)) 
    {
      showModal(modalDialog(
        title = 'Error!', result, footer = modalButton('Close')
      ))
      
      return()
    }
    
    # If 'eegfilt' has not had any problem, then, store result
    addFilteredSignalToUserData(dataSelected = dataSelected, signalName = input$filtSignal, data = result)

    # Show filter finish display
    showModal(modalDialog(
      title = 'Perfect!', paste('Signal', input$filtSignal,'has been filtered!'), footer = modalButton('Close')
    ))
    
    # When signal has been filtered, show all filtered signals to can show them
    output$selFiltSignalUI <- renderUI({ getSelFiltSignalUI() })
  })
  
  
  
  # Get FFT plot of signal 'signal'
  getFFTPlotOfSignal <- function(signal) 
  {
    # Get data filtered signal
    signalData <- getDataFilteredSignal(dataSelected, signal)
    srate <- getDataHeader(dataSelected)$outputRate
    # Calculate frequency response
    freq <- spec(signalData,f=srate,fftw=TRUE)
    # Convert kHz to Hz
    freq[,1] <- freq[,1]*1000
    # freq <- freq*1000
    
    # Cut FFT's until 40/7 Hz (EEG/EOG)
    if (regexpr('EEG', signal) > 0)
    {
      freq <- cutFFT(freq, 40)
    }
    else
    {
      freq <- cutFFT(freq, 7)
    }
    
    # Create list with needed fields to dygraph
    signalList <- list('Frequency' = freq[,1], 'Amplitude' = freq[,2])
    
    # Create dygraph
    plot <- dygraph(data = signalList, main = paste('Frequency Response',signal,sep=' - ')) %>%
      dyUnzoom() %>%
      dyRangeSelector() %>%
      dyAxis('x', drawGrid = FALSE, label = 'Frequency (Hz)') %>%
      dyAxis('y', label = 'Amplitude (microV^2)')
    
    # Return dygraph
    return(plot)
  }
  
  # Get selected signals of 'input$selectFiltSignals'
  getSelectedSignals <- reactive({
    # Check if 'input$selectFiltSignals' is null
    if (isNull(input$selectFiltSignals)) return()
    
    selectedSignals <- strsplit(input$selectFiltSignals, ' ')
    return(selectedSignals)
  })
  
  # Reactive which contains the signal's FFT plots for return to main server
  signalsFFTPlots <- reactive({
    selectedSignals <- getSelectedSignals()
    plot_output_list <- lapply(selectedSignals, function(signal) {
      fluidRow(
        renderDygraph({ getFFTPlotOfSignal(signal) }),
        hr()
      )
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
    
    return (plot_output_list)
  })
  
  # Return set signal's FFT plots
  return(signalsFFTPlots)
}