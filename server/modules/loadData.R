library(shiny)

# userData <<- list()

loadDataUI <- function(id) 
{
  ns <- NS(id)
  
  fluidPage(
    # Input files
    h3('Input files'),
    hr(),
    
    # Input Signal File -> *.txt
    fileInput(ns('signalFile'), 'Signal File', multiple = FALSE, accept = NULL, width = NULL),
    # Input TimeMarks File -> Test-*.txt
    fileInput(ns('timeMarks'), 'Time Marks File', multiple = FALSE, accept = NULL, width = NULL),
    
    # Load data button
    actionButton(ns('loadDataButton'), 'Load Data'),
    p('Click the button to load data.')
  )
}

loadData <- function(input, output, session) 
{
  # ObserveEvent for check correct params to load data
  observeEvent(input$loadDataButton, {
    # Check if input files have not been selected
    if (is.null(input$signalFile) || is.null(input$timeMarks)) 
    {
      showModal(modalDialog(
        title = 'Error!', 'You must select Signal and Time Marks files!', footer = modalButton('Close')
      ))
      
      return()
    }
    
    # Get input files
    signalFile <- input$signalFile
    timeMarks <- input$timeMarks
    
    # Get files's name
    signalName <- signalFile$name
    timeMarksName <- timeMarks$name
    
    # Check if input files are not corresponded
    if (timeMarksName != paste('Test', signalName, sep='-'))
    {
      showModal(modalDialog(
        title = 'Error!', 'Signal and Time Marks files must be corresponded!', footer = modalButton('Close')
      ))
      return()
    }
    
    # If input files are selected and are corresponded, load them
    
    # Show loading display
    showModal(modalDialog(
      title = 'Please wait!','Loading data...', footer = NULL
    ))
    
    # Load data
    task <- getTaskFromTXT(signalFile$datapath, timeMarks$datapath)
    signals <- task$Signals
    
    # Add data to userData
    addDataToUserData(signalName = signalName, data = task)
    
    # Show load finish display
    showModal(modalDialog(
      title = 'Perfect!', 'Data have been loaded!', footer = modalButton('Close')
    ))
  })
}