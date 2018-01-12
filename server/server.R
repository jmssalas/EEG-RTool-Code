library(shiny)
library(signal)
library(DT)
library(seewave)
library(dygraphs)

# Change the maxRequestSize to 100MB
options(shiny.maxRequestSize = 100*1024^2)

####################
# GLOBAL VARIABLES #
####################
dataSelected <<- NULL

if (!exists('userData'))
  userData <<- list()

shinyServer(function(input, output, session) {
  
  observeEvent(input$navbar, {
    if (input$navbar != 'Load Data' && length(userData) == 0)
    {
      showModal(modalDialog(
        title = 'Error!', 'You must load data first!'
      ))
      updateTabsetPanel(session, inputId = 'navbar', selected = 'Load Data')
      return()
    }
    
    if (input$navbar != 'Load Data' && input$navbar != 'Select Data' && is.null(dataSelected))
    {
      showModal(modalDialog(
        title = 'Error!', 'You must select data first!'
      ))
      updateTabsetPanel(session, inputId = 'navbar', selected = 'Select Data')
      return()
    }
    
    if (input$navbar == 'Ocular Correction')
    {
      for (signal in getSignalsToFilter(dataSelected))
      {
        if (!signalHasBeenFiltered(dataSelected, signal))
        {
          showModal(modalDialog(
            title = 'Error!', 'You must filter all signals first!'
          ))
          updateTabsetPanel(session, inputId = 'navbar', selected = 'Signal Filtering')
          return()
        }
      }
    }
  })
  

  ###################
  # LOAD DATA PANEL #
  ###################
  callModule(loadData, 'loadData')
  
  
  #####################
  # SELECT DATA PANEL #
  #####################
  callModule(selectData, 'selectData')
  
  
  #########################
  # DISPLAY SIGNALS PANEL #
  #########################
  signalsPlots <- callModule(displaySignals, 'displaySignals')
  output$plotSignals <- renderUI({ signalsPlots() })
  
  
  ##########################
  # SIGNAL FILTERING PANEL #
  ##########################
  signalsFFTPlots <- callModule(signalFiltering, 'signalFiltering')
  output$FFTPlotSignals <- renderUI({ signalsFFTPlots() })
  
  
  ###########################
  # OCULAR CORRECTION PANEL #
  ###########################
  correctedEEGPlots <- callModule(ocularCorrection, 'ocularCorrection')
  output$correctedEEGPlots <- renderUI({ correctedEEGPlots() })
  
  
  ######################################
  # CALCULATE BANDS SEGMENTATION PANEL # 
  ######################################
  callModule(calculateBandsSegmentation, 'calculateBandsSegmentation') 
  
  
  ############################
  # DISPLAY GPS SIGNAL PANEL # 
  ############################
  gpsPlot <- callModule(displayGPSSignal, 'displayGPSSignal')
  output$gpsPlot <- renderUI({ gpsPlot() })
  
  
  #################################
  # DISPLAY BANDS FILTERING PANEL # 
  #################################
  bandsPlots <- callModule(displayBandsFiltering, 'displayBandsFiltering')
  output$bandsPlots <- renderUI({ bandsPlots() })
  
  
  ###########################
  # CALCULATE INDICES PANEL #
  ###########################
  callModule(calculateIndices, 'calculateIndices')
  
  
  ######################################
  # DISPLAY VALUES OF REFERENCES PANEL # 
  ######################################
  valuesPlots <- callModule(displayValuesOfReferences, 'displayValuesOfReferences')
  output$valuesPlots <- renderUI({ valuesPlots() })
  
  
  #########################
  # DISPLAY INDICES PANEL # 
  #########################
  indicesPlots <- callModule(displayIndices, 'displayIndices')
  output$indicesPlots <- renderUI({ indicesPlots() })
  
  
  #############################
  # EXECUTE ALL PROCESS PANEL #
  #############################
  callModule(executeAllProcess, 'executeAllProcess')
  
  
  #######################
  # DOWNLOAD DATA PANEL #
  #######################
  callModule(exportIndices, 'downloadIndices')
  
  
  #######################
  # DOWNLOAD DATA PANEL #
  #######################
  callModule(exportSignals, 'downloadSignals')
  
})