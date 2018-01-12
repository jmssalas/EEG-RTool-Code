library(shiny)
library(DT)

exportIndicesUI <- function(id) 
{
  ns <- NS(id)
  
  fluidPage(
    p('Click rows to select data:'),
    br(),
    DT::dataTableOutput(ns('data')),
    hr(),
    downloadButton(ns('download'), 'Export Indices')
  )
}

exportIndices <- function(input, output, session)
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
  
  # ObserveEvent for select one userData.
  observeEvent(input$data_rows_selected, {
    row <- input$data_rows_selected

    # Get selected row
    rowSelected <- getUserData()[row,]

    # Get selected row's name
    dataToDownload <<- rownames(rowSelected)

    # Show dialog of data selecting
    showModal(modalDialog(
      title = 'Perfect!', paste('You have just selected', dataToDownload, 'data to export'), footer = modalButton('Close')
    ))
    
    output$download <- downloadHandler(
      filename = function() { paste(paste('indices', dataToDownload, Sys.Date(), sep='-'), 'csv', sep='.') },
      content = function(file) {
        indicesToCSV <- getIndicesToCSV(getIndices(dataToDownload))
        write.csv(indicesToCSV, file = file)
      }
    )
  })
}