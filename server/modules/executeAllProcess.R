library(shiny)

executeAllProcessUI <- function(id)
{
  ns <- NS(id)
  
  ns <- NS(id)
  fluidPage(
    h5(strong('Press the button for execute all process')),
    actionButton(ns('execute'), 'Execute All Process')
  )
}

executeAllProcess <- function(input, output, session)
{
  # ObserveEvent for execute button
  observeEvent(input$execute, {
    # SIGNAL FILTERING
    ## Params
    signals   <- getDataSignals(dataSelected)
    srate     <- getDataHeader(dataSelected)$outputRate
    
    ## Execute function
    showModal(modalDialog(
      title = 'Please wait!', 'Applying signals filtering...', footer = NULL
    )) 
    print(' > Applying signals filtering...')
    filtering      <- applyFiltering(signals, srate)
    print(' > Signals filtering applied.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'Signals filtering applied.', footer = modalButton('Close')
    ))
    
    ## Store result
    addSignalsFiltering(dataSelected, filtering)
    
    
    # OCULAR CORRECTION
    ## Params
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
    
    ## Execute function
    showModal(modalDialog(
      title = 'Please wait!', 'Applying ocular correction...', footer = NULL
    )) 
    print(' > Applying ocular correction...')
    filtering      <- applyGratton(eeg, eog)
    print(' > Ocular correction applied.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'Ocular correction applied.', footer = modalButton('Close')
    ))
    
    # Store result
    addCorrectedEEGToUserData(dataSelected = dataSelected, data = filtering$correctedEEG)
    addCorrectedEOGToUserData(dataSelected = dataSelected, data = filtering$EOGblinks)
    
    
    # SIGNALS SEGMENTATION
    ## Params
    tasks         <- getDataTaskPath(dataSelected)
    description   <- getDataDescription(dataSelected)
    signals       <- getCorrectedEEG(dataSelected)
    
    ## Execute function and store result
    showModal(modalDialog(
      title = 'Please wait!', 'Segmentating signals...', footer = NULL
    )) 
    print(' > Segmentating signals...')
    segmentation  <- calculateSignalsSegmentation(tasks, description, signals)
    print(' > Signals segmentated.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'Signals segmentated.', footer = modalButton('Close')
    ))
    
    # Store result
    addSignalsSegmentation(dataSelected, segmentation)
    
    
    # IAF CALCULATION
    ## Params
    closedEyes <- getClosedEyesSignal(dataSelected)
    
    # Execute function
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating IAF...', footer = NULL
    )) 
    print(' > Calculating IAF...')
    result     <- calculateIAF(closedEyes, srate)
    print(' > IAF calculated.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'IAF calculated.', footer = modalButton('Close')
    ))
    
    # Store result
    addGPSandIAFToUserData(data = dataSelected, gps = result$GPS, iaf = result$IAF)
    
    
    # BANDS FILTERING
    # Execute function
    showModal(modalDialog(
      title = 'Please wait!', 'Applying bands filtering...', footer = NULL
    )) 
    print(' > Applying bands filtering...')
    bandsFiltering <- calculateBandsFiltering(result$IAF, segmentation, srate)
    print(' > Bands filtering applied.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'Bands filtering applied.', footer = modalButton('Close')
    ))
    
    # Store result
    overrideSignalsSegmentation(dataSelected, bandsFiltering)
    
    
    # VALUES OF REFERENCE CALCULATION
    # Params 
    eegA <- getTasksOfSignal(dataSelected, 'Sensor-A:EEG')
    eegB <- getTasksOfSignal(dataSelected, 'Sensor-B:EEG')
    
    # Execute function
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating values of reference...', footer = NULL
    )) 
    print(' > Calculating values of reference...')
    values <- getValuesOfReference(eegA, eegB)
    print(' > Values of reference calculated.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'Values of reference calculated.', footer = modalButton('Close')
    ))
    
    # Store result
    addValuesOfReferences(dataSelected, values)
    
    
    # INDICES CALCULATION
    # Params
    tasksA       <- eegA$tasks
    tasksB       <- eegB$tasks
    references   <- values
    
    # Execute funtion
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating indices...', footer = NULL
    )) 
    print(' > Calculating indices...')
    indices <- getIndicesCalculation(tasksA, tasksB, references)
    print(' > Indices calculated.')
    print('')
    showModal(modalDialog(
      title = 'Perfect!', 'Indices calculated.', footer = modalButton('Close')
    ))
    
    # Store result
    addIndices(dataSelected, indices)
    
    showModal(modalDialog(
      title = 'Perfect!', 'All process has been executed!', footer = modalButton('Close')
    ))
  })
}