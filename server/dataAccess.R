getDataHeader <- function(data)
{
  return(userData[[data]]$task$Header)
}

getDataTaskPath <- function(data)
{
  return(userData[[data]]$task$`Task path`)
}

getDataDescription <- function(data)
{
  return(userData[[data]]$task$Description)
}

getDataSignals <- function(data)
{
  return(userData[[data]]$task$Signals)
}

getDataSignal <- function(data,signal)
{
  return(getDataSignals(data)[[signal]])
}

getDataFilteredSignals <- function(data)
{
  return(userData[[data]]$filteredSignals)
}

getDataFilteredSignal <- function(data, signal)
{
  return(getDataFilteredSignals(data)[[signal]]$`Smooth Data`)
}

signalHasBeenFiltered <- function(data, signal)
{
  filteredSignal <- getDataFilteredSignals(data)
  if (is.null(filteredSignal)) return(FALSE)
  if (is.null(filteredSignal[[signal]])) return(FALSE)
  return(TRUE)
}

getSignalsToFilter <- function(data)
{
  signals <- names(getDataSignals(data))
  toFilter <- c()
  for (signal in signals)
  {
    if (regexpr(':EEG', signal) > 0 ||  regexpr(':EOG', signal) > 0)
    {
      toFilter <- c(toFilter, signal)
    }
  }

  return(toFilter)
}

getSignalsWhichContains <- function(data, strSignal)
{
  toFilter <- getSignalsToFilter(data)
  signals <- c()
  for (signal in toFilter)
  {
    if (regexpr(strSignal, signal) > 0){
      signals <- c(signals, signal)
    }
  }
  return(signals)
}

getEEGSignals <- function(data)
{
  return(getSignalsWhichContains(data, ':EEG'))
}

getEOGSignals <- function(data)
{
  return(getSignalsWhichContains(data, ':EOG'))
}

getCorrectedEEG <- function(data)
{
  return(userData[[data]]$correctedEEG)
}

getEOGBlinks <- function(data)
{
  return(userData[[data]]$EOGblinks)
}

# getRowOfTask <- function(data, task) 
# {
#   # Get data description
#   dataDesc <- getDataDescription(data)
#   # For each row
#   for (i in seq(1:nrow(dataDesc)))
#   {
#     # If first column is '<closed_eyes>' return this row
#     if (dataDesc[i,1] == task) return(i)
#   }
#   # If it finishes here, '<closed_eyes>' has not been found.
#   return(paste(task,'has not been found'))
# }
# 
# # It suposes that EEG has been corrected and stored.
# getSignalsOfTask <- function(data,task)
# {
#   closedEyes <- list() # List which contains the result
#   
#   eeg <- getCorrectedEEG(data)    # Get corrected EEG
#   row <- getRowOfTask(data, task) # Get row of task
#   
#   # Get start and end mark
#   startMark <- getDataDescription(data)[row,4]
#   endMark   <- getDataDescription(data)[row,5]
#   
#   # For each signal
#   for (signal in names(eeg))
#   {
#     # Add range of closed_eyes 
#     closedEyes[[signal]] <- eeg[[signal]][startMark:endMark]
#   }
#   return(closedEyes)
# }

getIAFValue <- function(data)
{
  return(userData[[data]]$IAF)
}

getGPS <- function(data)
{
  return(userData[[data]]$GPS)
}

getSignalsSegmentation <- function(data)
{
  return(userData[[data]]$signalsSegmentation)
}

getTasksOfSignal <- function(data, signal)
{
  return(getSignalsSegmentation(data)[[signal]])
}

getValuesOfReferences <- function(data)
{
  return(userData[[data]]$references)
}

getIndices <- function(data)
{
  return(userData[[data]]$indices)
}

getClosedEyesSignal <- function(data)
{
  closedEyes <- list()
  signalSegmentation <- getSignalsSegmentation(data)
  for (signal in names(signalSegmentation))
  {
    closedEyes[[signal]] <- signalSegmentation[[signal]]$closed_eyes$signal
  }
 
  return(closedEyes)   
}

getTaskSignals <- function(data, signal, task, row)
{
  taskSignals <- list()
  signalsSegmentation <- getSignalsSegmentation(dataSelected)
  
  # Process '<closed_eyes>' task
  if (regexpr('closed_eyes', task) > 0)
  {
    for (signal in names(signalsSegmentation))
    {
      taskSignals[[signal]] <- signalsSegmentation[[signal]]$closed_eyes
    }
  }
  else
  {
    # Process '<basal>' task
    if (regexpr('basal', task) > 0)
    {
      # Get startMark of task
      startMark <- getDataDescription(dataSelected)[row,'startMark']
      basalName <- paste('basal', startMark, sep='-')
      for (signal in names(signalsSegmentation))
      {
        taskSignals[[signal]] <- signalsSegmentation[[signal]]$baselines[[basalName]]
      }
    }
    else
    {
      # Process others tasks
      for (signal in names(signalsSegmentation))
      {
        taskSignals[[signal]] <- signalsSegmentation[[signal]]$tasks[[task]]
      }
    }
  }
  
  return(taskSignals)
}


addDataToUserData <- function(signalName, data) 
{
  userData[[signalName]] <<- list(task = data)
}

addSignalsFiltering <- function(dataSelected, signalsFiltering)
{
  userData[[dataSelected]]$filteredSignals <<- signalsFiltering
}


addFilteredSignalToUserData <- function(dataSelected, signalName, data) 
{
  userData[[dataSelected]]$filteredSignals[[signalName]] <<- data
}

addSignalsSegmentation <- function(data, signals)
{
  userData[[data]]$signalsSegmentation <<- signals
}

addCorrectedEEGToUserData <- function(dataSelected, data) 
{
  userData[[dataSelected]]$correctedEEG <<- data
}

addCorrectedEOGToUserData <- function(dataSelected, data) 
{
  userData[[dataSelected]]$EOGblinks <<- data
}

addGPSandIAFToUserData <- function(data, gps, iaf)
{
  userData[[data]]$GPS <<- gps
  userData[[data]]$IAF <<- iaf
}

overrideSignalsSegmentation <- function(data, newSignalsSegmentation)
{
  userData[[data]]$signalsSegmentation <<- newSignalsSegmentation
}

addValuesOfReferences <- function(data, references)
{
  userData[[data]]$references <<- references
  # userData[[data]]$references[[basal]]$startMark <<- startMark
  # userData[[data]]$references[[basal]]$values <<- values
}

addIndices <- function(data, indices)
{
  userData[[data]]$indices <<- indices
}
