library(shiny)

calculateBandsSegmentationUI <- function(id)
{
  ns <- NS(id)
  fluidPage(
    h5(strong('Press the button for calculate bands segmentation')),
    actionButton(ns('calculate'), 'Calculate Bands Segmentation')
  )
}

calculateBandsSegmentation <- function(input, output, session)
{
  # Function which segment the signals by tasks and
  # add them in to userData
  segmentTheSignals <- function()
  {
    # Get Task Path of dataSelected
    tasks         <- getDataTaskPath(dataSelected)
    # Get Description of dataSelected
    description   <- getDataDescription(dataSelected)
    # Get corrected EEG of dataSelected
    signals       <- getCorrectedEEG(dataSelected)
    # Get segmentation signals
    segmentation  <- calculateSignalsSegmentation(tasks, description, signals)
    # Add segmentation to userData
    addSignalsSegmentation(dataSelected, segmentation)
  }
  
  # Function which gets IAF value and GPS value, and store them into userData
  getIAF <- function()
  {
    # Get closedEyes signal
    closedEyes <- getClosedEyesSignal(dataSelected)
    # Get srate
    srate      <- getDataHeader(dataSelected)$outputRate
    # Calculate IAF
    result     <- calculateIAF(closedEyes, srate)
    # Add GPS and IAF to UserData of 'dataSelected'
    addGPSandIAFToUserData(data = dataSelected, gps = result$GPS, iaf = result$IAF)
  }
  
  # Function which does the bands filtering for all tasks
  bandsFiltering <- function()
  {
    # Get IAF value
    iaf                 <- getIAFValue(dataSelected)
    # Get signals segmentation
    signalsSegmentation <- getSignalsSegmentation(dataSelected)
    # Get sample rate
    srate               <- getDataHeader(dataSelected)$outputRate
    # Calculate bands filtering
    newSignals          <- calculateBandsFiltering(iaf, signalsSegmentation, srate)
    # Override signals segmentation of userData
    overrideSignalsSegmentation(dataSelected, newSignals)
  }
  
  # ObserveEvent for calculate button
  observeEvent(input$calculate, {
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating signals segmentation...', footer = NULL
    )) 
    # Calculate Signal segmentation
    segmentTheSignals()
    
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating IAF value...', footer = NULL
    )) 
    # Calculate IAF value
    getIAF()
    
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating bands filtering...', footer = NULL
    )) 
    # Calculate Bands filtering
    bandsFiltering()
    showModal(modalDialog(
      title = 'Perfect!', 'Bands segmentation has been finished', footer = modalButton('Close')
    ))
  })
}