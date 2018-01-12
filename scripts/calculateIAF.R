library(signal)
library(seewave)
library(SDMTools)

# Calculate PSD (Power Spectral Density)
# Return a list with PSD for each signal in 'signals' with 'srate' sampleRate
calculatePSD <- function(signals, srate) 
{
  noverlap <- 10 # Superposición de la ventana para la PSD
  Fres = 0.1 # Marcas de frecuencia resultado (La marcas finales será una cada Fres Hz)
  window = hanning(round(srate/Fres)) # Creación de a ventana para la reducción del ruido
  nfft = length(window) # Número de marcas que aparecerán en la fft
  
  psd <- list()
  for (signal in names(signals))
  {
    # Calculate PSD
    # freq <- meanspec(wave=signals[[signal]], f=srate, wl=nfft, ovlp=noverlap, PSD = TRUE, fftw = TRUE)
    freq <- spec(signals[[signal]], f=srate, wl=nfft, PSD=TRUE, fftw=TRUE)
    # Convert kHz to Hz
    freq[,1] <- freq[,1]*1000
    # freq <- freq*1000
    
    # Store PSD
    psd[[signal]] <- freq
  }
  return(psd)
}

# Get the first value higher or equals than 'value' into 'data'.
# It suposes that 'data' is a vector
getTheFirstHigherOrEqualsThanValue <- function(data, value)
{
  for (i in 1:length(data))
  {
    if (data[i] >= value) return(i)
  }
}

# Get the last value lower or equals than 'value' into 'data'.
# It suposes that 'data' is a vector
getTheLastLowerOrEqualsThanValue <- function(data, value)
{
  for (i in 1:length(data))
  {
    if (data[i] >= value) return(i)
  }
}

# Extract alpha band of PSD 'psd'
# Return a list with alpha band for each signal in 'psd'
extractAlphaBand <- function(psd) 
{
  alphaBand <- list()
  for (signal in names(psd))
  {
    # Get lower limit of psd[[signal]]
    lowerLimit  <- getTheFirstHigherOrEqualsThanValue(psd[[signal]][,1], 7.5)
    # Get higher limit of psd[[signal]]
    higherLimit <- getTheLastLowerOrEqualsThanValue(psd[[signal]][,1], 12.5)
    # Store alpha band
    alphaBand[[signal]] <- psd[[signal]][lowerLimit:higherLimit,]
  }
  return(alphaBand)
}

# Calculate GPS of alpha band 'alphaBand'
# Return a matrix with GPS calculated
calculateGPS <- function(alphaBand)
{
  # Get amount of signals
  nSignals <- length(names(alphaBand))
  # Initialize gps 
  gps <- alphaBand[[1]]
  gps[,2] <- 0
  
  # For each signal in 'alphaBand'
  for (signal in alphaBand)
  {
    # Add into gps
    gps[,2] <- gps[,2] + signal[,2]
  }
  # Calculate mean
  gps[,2] <- gps[,2]/nSignals
  return(gps)
}

# Check if 'signal' is a decreasing signal.
# If it is, return TRUE, otherwise return FALSE
isDecreasingSignal <- function(signal)
{
  previous <- signal[1]
  for (actual in signal[2:length(signal)])
  {
    if (actual > previous) return(FALSE)
    previous <- actual
  }
  return(TRUE)
}

# Check if 'signal' is a increasing signal.
# If it is, return TRUE, otherwise return FALSE
isIncreasingSignal <- function(signal)
{
  previous <- signal[1]
  for (actual in signal[2:length(signal)])
  {
    if (actual < previous) return(FALSE)
    previous <- actual
  }
  return(TRUE)
}

# Get frequency of value 'value' into GPS 'gps' signal
getFrequencyOfValue <- function(gps, value) 
{
  for (i in 1:nrow(gps))
  {
    if (gps[i,][[2]] == value) return(gps[i,][[1]])
  }
}

# Calculate IAF with 'closedEyes' data and 'srate' sampleRate
# Return a list with two elements:
#   -> GPS: GPS signal
#   -> IAF: IAF value
calculateIAF <- function(closedEyes, srate)
{
  # Calculate PSD
  psd <- calculatePSD(closedEyes, srate)
  # Extract alpha band
  alphaBand <- extractAlphaBand(psd)
  # Calculate GPS
  gps <- calculateGPS(alphaBand)
  
  if (!isDecreasingSignal(gps[,2]) && !isIncreasingSignal(gps[,2]))
  {
    # If GPS is not a decreasing and is not a increasing signal,
    # store IAF like max of GPS signal
    iaf <- getFrequencyOfValue(gps, max(gps[,2]))
  }
  else 
  {
    # If GPS is a decreasing or is a increasing signal,
    # sotre IAF like Center of Gravity of GPS signal
    iaf <- COGravity(gps[,1])[[1]]
  }
  
  return(list(GPS = gps, IAF = iaf))
}