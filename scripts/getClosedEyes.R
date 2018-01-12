getClosedEyes <- function(segmentation)
{
  closedEyes <- list()
  for (signal in names(segmentation))
  {
    closedEyes[[signal]] <- segmentation[[signal]]$closed_eyes$signal
  }
  
  return(closedEyes)   
}