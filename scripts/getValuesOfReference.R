getValuesOfReference <- function(eegA, eegB)
{
  baselines <- list()
  
  # For each baseline, get values of references for indices
  for (basal in names(eegA$baselines))
  {
    baselines[[basal]]$values    <- getValuesOfReferencesForIndices(rightBasalData = eegA$baselines[[basal]], leftBasalData = eegB$baselines[[basal]])
    baselines[[basal]]$startMark <- eegA$baselines[[basal]]$startMark
  }
  
  return(baselines)
}