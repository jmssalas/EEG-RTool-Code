# openSession <- function(path, file) {
#   signalsFilePath = paste(path, file, '.txt', sep="");
#   file <- readLines(signalsFilePath);
#   file
# }

openSession <- function(filepath) {
  file <- readLines(filepath);
  file
}

getHeader <- function(file) {
  # Name
  nameLine = file[regexpr('Session:\t', file) > 0];
  name = substr(nameLine, nchar('Session:\t')+1, nchar(nameLine));

  # Time
  timeLine = file[regexpr('Time:\t', file) > 0];
  timeStr = substr(timeLine, nchar('Time:\t')+1, nchar(timeLine));
  
  # Duration
  durationLine = file[regexpr('Duration:\t', file) > 0];
  durationStr = substr(durationLine, nchar('Duration:\t')+1, nchar(durationLine)-nchar('\tSeconds.'));
  
  # Output Rate
  outputRateLine = file[regexpr('Output rate:\t', file) > 0];
  outputRateStr = substr(outputRateLine, nchar('Output rate:\t')+1, nchar(outputRateLine)-nchar('\tSamples/sec.'));
  
  date = as.POSIXct(timeStr, format ='%H:%M:%S')
  duration = as.integer(durationStr);
  outputRate = as.integer(outputRateStr);
  
  header <- list(name = name, date = date, duration = duration, outputRate = outputRate)
  header
}

getChannels <- function(file) {
  # Channels names
  channelsLine <- file[regexpr('Sensor', file) > 0]
  channels <- list(names = unlist(strsplit(channelsLine, ',')))
  
  # '[G] Heart Rate' name has been changed to '[G]Heart-Rate' because the 'checkboxGroupInput' of Display Signal Panel
  # separates the selected values by spaces (' '). To show some signals, I use 'strsplit' function with separate=' ',
  # so that this name split too.
  channels$names[channels$names == '[G] Heart Rate'] <- '[G]Heart-Rate'
  
  
  # Channels SPS
  channelsSPSLine <- file[regexpr('SPS', file) > 0]
  channelsSPS <- unlist(strsplit(channelsSPSLine, ','))
  channelsSPS <- unlist(strsplit(channelsSPS, ' '))
  channelsSPS <- as.integer(channelsSPS[seq(1,length(channelsSPS),2)])
  channels$SPS <- channelsSPS
  
  # Channels values
  channelsValuesLines <- file[(regexpr('[0-9]\\.', file) > 0)]
  channelsValues <- strsplit(channelsValuesLines, ',')
  channelsValues <- data.frame(matrix(as.numeric(unlist(channelsValues)), length(channelsValues), length(channels$names), T))
  names(channelsValues) <- channels$names
  channels$values <- channelsValues
  
  channels
}

# importFromTXT <- function(path, file) {
#   file <- openSession(path, file)
#   data = list();
#   data$header <- getHeader(file)
#   data$channels <- getChannels(file)
#   
#   data
# }

importFromTXT <- function(filepath) {
  file <- openSession(filepath)
  data = list();
  data$header <- getHeader(file)
  data$channels <- getChannels(file)
  
  data
}