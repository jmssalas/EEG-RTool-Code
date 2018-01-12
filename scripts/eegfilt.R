library(signal)

# Function which print error message on stderr()
printError <- function(...) { cat(sprintf(...), sep='', file=stderr()) }

#' eegfilt() -  (high|low|band)-pass filter data using two-way least-squares 
#'              FIR filtering. Optionally uses the window method instead of 
#'              least-squares. 
#' Usage:
#'  >> eegfilt(data,srate,locutoff,hicutoff);
#'  >> eegfilt(data,srate,locutoff,hicutoff,epochframes,filtorder,causal);
#'
#' Inputs:
#'   data        = vector data to filter
#'   srate       = data sampling rate (Hz)
#'   locutoff    = low-edge frequency in pass band (Hz)  {0 -> lowpass}
#'   hicutoff    = high-edge frequency in pass band (Hz) {0 -> highpass}
#'   epochframes = frames per epoch (filter each epoch separately {def/0: data is 1 epoch}
#'   filtorder   = length of the filter in points {default 3*fix(srate/locutoff)}
#'   causal      = [FALSE|TRUE] use causal filter if set to TRUE (default FALSE)
#'
#' Output:
#'    If there are no errors, a list with following fields:
#'      >> `Smooth Data`  = smoothed data
#'      >> `Coefficients` = filter coefficients [smoothdata <- filtfilt(filtwts,data)]
#'    otherwise:
#'      >> A string with the error
eegfilt <- function(data, srate, locutoff, hicutoff, epochframes = 0, filtorder = 0, causal = FALSE) 
{
  if (!is.vector(data))
  {
    error = 'Error: input data should be a vector.'
    printError(error)
    return(error)
  }
  
  frames  <- length(data)
  
  nyq           <- srate*0.5   # Nyquist frequency
  MINFREQ       <- 0
  
  minfac        <- 3     # This many (lo)cutoff-freq cycles in filter
  min_filtorder <- 15    # Minimum filter length
  trans         <- 0.15  # Fractional width of transition zones
  
  if (locutoff > 0 && hicutoff > 0 && locutoff > hicutoff) 
  {
    error <- 'Error: locutoff > hicutoff?'
    printError(error)
    return(error)
  }
  
  if (locutoff < 0 || hicutoff < 0) 
  {
    error <- 'locutoff or hicutoff < 0?'
    printError(error)
    return(error)
  }
  
  if (locutoff > nyq) 
  {
    error <- 'Low cutoff frequency cannot be > srate/2'
    printError(error)
    return(error)
  }
  
  if (hicutoff > nyq) 
  {
    error <- 'High cutoff frequency cannot be > srate/2'
    printError(error)
    return(error)
  }
  
  if (filtorder == 0)
  {
    if (locutoff > 0) 
    {
      filtorder = minfac*trunc(srate/locutoff)
    }
    else if (hicutoff > 0) 
    {
      filtorder = minfac*trunc(srate/hicutoff)
    }
      
    if (filtorder < min_filtorder) 
    {
      filtorder = min_filtorder
    }
  }
  
  if (epochframes == 0)
  {
    epochframes = frames # Default
  }
  
  epochs <- trunc(frames/epochframes)
  if (epochs*epochframes != frames)
  {
    error <- 'epochframes does not divide frames'
    printError(error)
    return(error)
  }
  
  if (filtorder*3 > epochframes)  # filfilt() restriction
  {
    error <- sprintf('eegfilt(): filter order is %d. epochframes is %d. epochframes must be at least 3 times the filtorder.', filtorder, epochframes)
    printError(error)
    return(error)
  }
  
  if ((1+trans)*hicutoff/nyq > 1) 
  {
    error <- 'high cutoff frequency too close to Nyquist frequency'
    printError(error)
    return(error)
  }
  
  bandEdges <- NULL
  type <- NULL
  
  if (locutoff > 0 && hicutoff > 0)  # Bandpass filter
  {
    sprintf('eegfilt() - performing %d-point bandpass filtering.', filtorder)
    sprintf('            If a message, "Matrix is close to singular or badly scaled," appears,');
    sprintf('            then R has failed to design a good filter. As a workaround,');
    sprintf('            for band-pass filtering, first highpass the data, then lowpass it.');
    
    bandEdges <- c(locutoff, hicutoff)/(srate/2)
    type <- 'pass'
  }
  else if (locutoff > 0) # Highpass filter
  {
    if (locutoff/nyq < MINFREQ)
    {
      error <- sprintf('eegfilt() - highpass cutoff freq must be > %g Hz', MINFREQ*nyq)
      printError(error)
      return(error)
    }
    
    sprintf('eegfilt() - performing %d-point highpass filtering.', filtorder)
    bandEdges <- locutoff/(srate/2)
    type <- 'high'
  }
  else if (hicutoff > 0) # Lowpass filter
  {
    if (hicutoff/nyq < MINFREQ)
    {
      error <- sprintf('eegfilt() - lowpass cutoff freq must be > %g Hz', MINFREQ*nyq)
      printError(error)
      return(error)
    }
    
    sprintf('eegfilt() - performing %d-point lowpass filtering.', filtorder)
    bandEdges <- hicutoff/(srate/2)
    type <- 'low'
  }
  else
  {
    error <- 'You must provide a non-0 low or high cut-off frequency'
    printError(error)
    return(error)
  }
  
  filtwts <- fir1(filtorder, bandEdges, type=type)
  
  smoothdata <- data
  for (e in 1:epochs)
  {
    range <- ((e-1)*epochframes+1):(e*epochframes)
    
    tryCatch(
      expr = {
        if (causal) smoothdata[range] <- filter(  filtwts, data[range])
        else        smoothdata[range] <- filtfilt(filtwts, data[range])
      }, 
      error = function(err) 
      {
        if (causal) smoothdata[range] <- filter(  filtwts, as.double(data[range]))
        else        smoothdata[range] <- filtfilt(filtwts, as.double(data[range]))
      }, 
      finally = {}
    )
  }
  
  output <- list()
  output$`Smooth Data` <- smoothdata
  output$`Coefficients` <- filtwts

  output
}