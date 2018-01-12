# Function which print error message on stderr()
printError <- function(...) { cat(sprintf(...), sep='', file=stderr()) }


#' gratton() - Applies Ocular Correction as specified by Gratton et al.,
#'             1983 to the EEG-data using channel eog for regression.
#'
#' Usage:
#'   >>  ceeg = gratton(eeg, eog);
#'
#' Inputs:
#'   eeg     = to-be-corrected data in the format (data.frame):
#'             Time x Electrode
#'             such that eeg(400, 2) is the sampling point
#'             400 of 2nd electrode
#'   eog     = data of the eog-channel -- it is a vector data
#'   
#' Outputs:
#'   Return a list with the following fields:
#'     -> ceeg    = corrected EEG data (same format as input)
#'     -> neog    = EOG data with the subject's blinks (like data.frame)
gratton <- function(eeg, eog) {
  # Check if 'eeg' param is a matrix
  if (!is.data.frame(eeg)) 
  {
    error = 'Error: input eeg should be a data.frame.'
    printError(error)
    return(error)
  }
  
  # Get electrodes and times
  el      <- ncol(eeg)
  times   <- nrow(eeg)
  ceeg    <- eeg
  
  # Inicialize variables
  neog  <- eog
  neeg  <- eeg
  
  # loop through electrodes and get the K's
  Kblinks <- c()  # coefficients within blinks
  x = neog
  
  # correction within blinks if appropiate
  for (e in 1:el) 
  {
    y <- neeg[,e]
    
    data <- matrix(c(x,y), ncol = 2)
    colnames(data) <- c('x','y')
    
    lm <- lm(y ~ x, data.frame(data))
    Kblinks <- c(Kblinks, lm$coefficients[2])
  }
  
  minus <- kronecker(matrix(1,nrow=times), matrix(Kblinks, ncol = el)) * eog
  ceeg <- eeg - minus
  
  return(list(ceeg = ceeg, neog = data.frame(minus)))
}