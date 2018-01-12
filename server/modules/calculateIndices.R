library(shiny)

calculateIndicesUI <- function(id)
{
  ns <- NS(id)
  
  ns <- NS(id)
  fluidPage(
    h5(strong('Press the button for calculate indices')),
    actionButton(ns('calculate'), 'Calculate Indices')
  )
}

calculateIndices <- function(input, output, session)
{
  # ObserveEvent for calculate button
  observeEvent(input$calculate, {
    eegA <- getTasksOfSignal(dataSelected, 'Sensor-A:EEG')
    eegB <- getTasksOfSignal(dataSelected, 'Sensor-B:EEG')
    
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating values of reference...', footer = NULL
    )) 
    
    references <- getValuesOfReference(eegA, eegB)
    addValuesOfReferences(dataSelected, references)
    
    showModal(modalDialog(
      title = 'Please wait!', 'Calculating indices...', footer = NULL
    )) 
    indices <- getIndicesCalculation(eegA$tasks, eegB$tasks, getValuesOfReferences(dataSelected))
    addIndices(dataSelected, indices)
    
    showModal(modalDialog(
      title = 'Perfect!', 'Indices calculation has been finished', footer = modalButton('Close')
    ))
  })
}