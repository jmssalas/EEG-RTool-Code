applyGratton <- function(eeg, eog)
{
  eegMatrix = convertListToMatrix(eeg)
  gratton <- gratton(eegMatrix, eog)
  
  filtering <- list()
  filtering$correctedEEG <- convertMatrixToList(gratton$ceeg)
  filtering$EOGblinks <- gratton$neog
  return(filtering)
}