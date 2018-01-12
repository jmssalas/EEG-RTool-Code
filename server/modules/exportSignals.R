library(shiny)

exportSignalsUI <- function(id) 
{
  ns <- NS(id)
  
  fluidPage(
    p('Click rows to select data:'),
    br(),
    DT::dataTableOutput(ns('data')),
    hr(),
    radioButtons(ns('signals'), label = 'Select signals which you want to download', 
                       choices = c('Normal Signals', 'Filtered Signals', 'Corrected Signals'), 
                       inline = TRUE),
    hr(),
    downloadButton(ns('download'), 'Export Signals')
  )
}

exportSignals <- function(input, output, session)
{
  # Get 'userData' for show table with all user data
  getUserData <- reactive({
    dataToDownload <<- NULL
    
    list <- list()
    
    # Foreach userData create its row
    for (data in names(userData))
    {
      if (!is.null(getIndices(data)))
      {
        list[[data]] <- getDataHeader(data)
        # Convert 'date' to string
        list[[data]]$date <- format(list[[data]]$date)
      }
    }
    
    # Convert list to data.frame
    df <- do.call(rbind, lapply(list, data.frame, stringsAsFactors=FALSE))
    
    colnames(df) <- c('Name', 'Date', 'Duration', 'Output Rate')
    return(df)
  })
  
  # Render Data table
  output$data = DT::renderDataTable(getUserData(), server = TRUE, selection = 'single',
                                    options = list(columnDefs = list(list(className = 'dt-center')))
  )
  
  getSignalsToCSV <- reactive({
    # Check if 'input$signals' is null
    if (is.null('dataToDownload')) return()
    
    if (input$signals == 'Normal Signals')
    {
      return(getDataSignals(dataToDownload))
    }
    if (input$signals == 'Filtered Signals')
    {
      filtered <- list()
      for (signal in names(getDataFilteredSignals(dataToDownload)))
      {
        filtered[[signal]] <- getDataFilteredSignal(dataToDownload, signal)
      }
      return(filtered)
    }
    if (input$signals == 'Corrected Signals')
    {
      return(list(EEG <- getCorrectedEEG(dataToDownload), EOGBlinks = getEOGBlinks(dataToDownload)))
    }
  }) 
  
  # ObserveEvent for select one userData.
  observeEvent(input$data_rows_selected, {
    row <- input$data_rows_selected
    
    # Get selected row
    rowSelected <- getUserData()[row,]
    
    # Get selected row's name
    dataToDownload <<- rownames(rowSelected)
    
    # Show dialog of data selecting
    showModal(modalDialog(
      title = 'Perfect!', paste('You have just selected', dataToDownload, 'data to download'), footer = modalButton('Close')
    ))
    
    output$download <- downloadHandler(
      filename = function() { paste(paste(input$signals,dataToDownload, Sys.Date(), sep='-'), 'csv', sep='.') },
      content = function(file) {
        indicesToCSV <- getSignalsToCSV()
        write.csv(indicesToCSV, file = file)
      }
    )
  })
}