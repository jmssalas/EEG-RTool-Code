doFiltering <- function(data, srate, locutoff, hicutoff)
{
  # High-pass filtering
  highpass <- eegfilt(data = data, srate = srate, 
                      locutoff = locutoff, hicutoff = 0)
  
  # Check if 'eegfilt' has had any problem
  if (!is.list(highpass)) 
  {
    print(highpass)
    return()
  }
  
  # Low-pass filtering
  data <- highpass$`Smooth Data`
  result <- eegfilt(data = data, srate = srate, 
                    hicutoff = hicutoff, locutoff = 0)
  
  # Check if 'eegfilt' has had any problem
  if (!is.list(result)) 
  {
    print(result)
    return()
  }
  
  filtered <- result$`Smooth Data`
  return(filtered)
}

processClosedEyes <- function(signalsSegmentation, bands, srate)
{
  closedEyes <- list()
  for (signal in names(signalsSegmentation))
  {
    closed_eyes <- signalsSegmentation[[signal]]$closed_eyes
    closedEyes[[signal]]$signal <- closed_eyes$signal
    closedEyes[[signal]]$startMark <- closed_eyes$startMark
    
    for (band in names(bands))
    {
      data <- closed_eyes$signal
      filtered <- doFiltering(data, srate, bands[[band]]$locutoff, bands[[band]]$hicutoff)
      closedEyes[[signal]][[band]] <- filtered
    }
  }

  return(closedEyes)
}

processBaselines <- function(signalsSegmentation, bands, srate)
{
  baselines <- list()
  for (signal in names(signalsSegmentation))
  {
    BS <- signalsSegmentation[[signal]]$baselines
    for (basal in names(BS))
    {
      baseline <- BS[[basal]]
      baselines[[signal]][[basal]]$signal <- baseline$signal
      baselines[[signal]][[basal]]$startMark <- baseline$startMark
      
      for (band in names(bands))
      {
        data <- baseline$signal
        filtered <- doFiltering(data, srate, bands[[band]]$locutoff, bands[[band]]$hicutoff)
        baselines[[signal]][[basal]][[band]] <- filtered
      }
    }
  }
  
  return(baselines)
}

processTasks <- function(signalsSegmentation, bands, srate)
{
  tasks <- list()
  
  for (signal in names(signalsSegmentation))
  {
    TASKS <- signalsSegmentation[[signal]]$tasks
    for (task in names(TASKS))
    {
      taskData <- TASKS[[task]]
      tasks[[signal]][[task]]$signal    <- taskData$signal
      tasks[[signal]][[task]]$startMark <- taskData$startMark
      
      for (band in names(bands))
      {
        data <- taskData$signal
        filtered <- doFiltering(data, srate, bands[[band]]$locutoff, bands[[band]]$hicutoff)
        tasks[[signal]][[task]][[band]] <- filtered
      }
    }
  }
  
  return(tasks)
}


# Function which calculate bands filtering.
# Its params are:
#  -> iaf: IAF value.
#  -> signals: Signals segmentation.
#  -> srate: Sample rate of signals.
#
# Return 'signals' with the bands filtering added for each task.
calculateBandsFiltering <- function(iaf, signals, srate)
{
  # Create bands list with locutoff-hicutoff for each bands
  bands <- list(
    theta = list(locutoff = iaf-6, hicutoff = iaf-2),
    alpha = list(locutoff = iaf-2, hicutoff = iaf+2),
    beta  = list(locutoff = iaf+2, hicutoff = 30)
  )
  # Make copy of signals
  signalsSegmentation <- signals
  
  closed_eyes <- processClosedEyes(signalsSegmentation, bands, srate)
  baselines   <- processBaselines(signalsSegmentation, bands, srate)
  tasks       <- processTasks(signalsSegmentation, bands, srate)
  
  # For each signal store its results
  for (signal in names(signalsSegmentation))
  {
    signals[[signal]]$closed_eyes <- closed_eyes[[signal]]
    signals[[signal]]$baselines   <- baselines[[signal]]
    signals[[signal]]$tasks       <- tasks[[signal]]
  }
  
  return(signals)
}