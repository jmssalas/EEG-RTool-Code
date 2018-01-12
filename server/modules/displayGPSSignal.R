library(shiny)

displayGPSSignalUI <- function(id)
{
  ns <- NS(id)
  
  uiOutput(ns('output'))
}

displayGPSSignal <- function(input, output, session)
{
  gpsPlot <- reactive({
    gps <- getGPS(dataSelected); iaf <- getIAFValue(dataSelected)
    
    ui <- fluidRow(
      renderDygraph({ 
        # Create list with needed fields to dygraph
        signalList <- list('Frequency' = gps[,1], 'Amplitude' = gps[,2])
        
        # Create dygraph
        plot <- dygraph(data = signalList, main = 'GPS - Global Power Spectrum') %>%
          dyUnzoom() %>%
          dyRangeSelector() %>%
          dyAxis('x', drawGrid = FALSE, label = 'Frequency (Hz)') %>%
          dyAxis('y', label = 'Amplitude (microV^2)') %>%
          dyAnnotation(iaf, paste0('IAF - ',round(iaf,2)), width = 75, height = 25)
        
        return(plot)
      }),
      hr()
    )
    return (ui)
  })
  
  return(gpsPlot)
}