applyFiltering <- function(signals, srate)
{
  filtered <- list()
  print('  >> Filtering signals...')
  for (signal in names(signals))
  {
    if (regexpr(':EEG',signal) > 0)
    {
      filtered[[signal]] <- eegfilt(data = signals[[signal]], srate = srate, locutoff = 2, hicutoff = 30)
      print(paste('    >>>', signal,'filtered'))
    }
    else if (regexpr(':EOG', signal) > 0)
    {
      filtered[[signal]] <- eegfilt(data = signals[[signal]], srate = srate, locutoff = 0.1, hicutoff = 5)
      print(paste('    >>>', signal,'filtered'))
    }
  }
  return(filtered)
}