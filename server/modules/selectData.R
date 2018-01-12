library(shiny)
library(DT)

selectDataUI <- function(id) 
{
  ns <- NS(id)
  
  fluidPage(
    p('Click rows to select data:'),
    br(),
    DT::dataTableOutput(ns('data'))
  )
}

selectData <- function(input, output, session)
{
  # Get 'userData' for show table with all user data
  getUserData <- reactive({
    list <- list()
    
    # Foreach userData create its row
    for (data in names(userData))
    {
      list[[data]] <- getDataHeader(data)
      # Convert 'date' to string
      list[[data]]$date <- format(list[[data]]$date)
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
    dataSelected <<- rownames(rowSelected)
    
    # Show dialog of data selecting
    showModal(modalDialog(
      title = 'Perfect!', paste('You have just selected', dataSelected, 'data'), footer = modalButton('Close')
    ))
  })
}