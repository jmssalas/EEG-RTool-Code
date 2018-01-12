secsToSamples <- function(secs, sampleRate) {
  secs*sampleRate
}

timeDiff <- function(time1, time2) {
  secs1 <- as.numeric(format(time1, '%H'))*3600 + as.numeric(format(time1, '%M'))*60 + as.numeric(format(time1, '%S'));
  secs2 <- as.numeric(format(time2, '%H'))*3600 + as.numeric(format(time2, '%M'))*60 + as.numeric(format(time2, '%S'));
  
  secs2-secs1
}

pathToNames <- function(paths) {
  otherIndexes <- grep('<', paths)
  imageNames <- paths[-otherIndexes]
  imageNames2 <- unlist(strsplit(unlist(strsplit(imageNames, '\\\\')), '\\.'))
  imageNames2 <- imageNames2[seq(2,length(imageNames2), 3)]
  taskNames <- character(length=length(paths))
  taskNames[otherIndexes] <- paths[otherIndexes]
  taskNames[taskNames==""] <- imageNames2
  
  taskNames
}

timesToMarks <- function(sampleRate, starTime, times) {
  marks <- data.frame(startMark = secsToSamples(timeDiff(starTime, times$startTime), sampleRate),
                      endMark = secsToSamples(timeDiff(starTime, times$endTime), sampleRate))
  
  marks
}

resample <- function(originalRate, newRate, signal) {
  ratio <- originalRate/newRate
  newLength <- length(signal)/ratio
  newSignal <- vector(mode = "numeric", length = newLength)
  for (i in 1:newLength) {
    index0 <- (i-1)*ratio + 1
    index1 <- i*ratio
    
    segment <- signal[index0:index1]
    newSignal[i] <- mean(segment)
  }
  newSignal
}

resampleSignals <- function(channels) {
  sampleRates <- channels$SPS
  channelsValues <- channels$values
  newSignals <- list()
  index <- 1
  
  for (i in names(channelsValues)) {
    signal <- channelsValues[[i]]
    if(sampleRates[index] != 256)
      newSignals[[i]] <- resample(256, sampleRates[index], signal)
    else
      newSignals[[i]] <- signal
    index <- index + 1
  }
  
  newSignals
}

# getTaskFromTXT <- function(path, timeMarks, signalsFile) {
#   signals <- importFromTXT(path, signalsFile)
#   sessionInfo <- getSessionInfo(path, timeMarks, signalsFile)
#   
#   session <- list()
#   
#   # Header
#   
#   session$`Header` <- signals$header
#   
#   # Task path to Task names
#   
#   session$`Task path` <- sessionInfo$`Task path`
#   # session$`Task name` <- pathToNames(sessionInfo$`Task path`)
#   
#   # Times
#   
#   # session$Times <- data.frame(startTime = sessionInfo$`Start time`, endTime = sessionInfo$`End time`)
#   times <- data.frame(startTime = sessionInfo$`Start time`, endTime = sessionInfo$`End time`)
#   
#   # Times to marks
#   
#   # session$`Marks` <- timesToMarks(signals$header$outputRate, signals$header$date, session$Times)
#   marks <- timesToMarks(signals$header$outputRate, signals$header$date, times)
#   
#   session$`Description` <- data.frame(taskName = pathToNames(sessionInfo$`Task path`),
#                                       startTime = format(times$startTime, '%H:%M:%S'),
#                                       endTime = format(times$endTime, '%H:%M:%S'),
#                                       startMark = marks$startMark,
#                                       endMark = marks$endMark)
#   
#   # Signals
#   session$`Signals` <- resampleSignals(signals$channels)
#   
#   session
# }

getTaskFromTXT <- function(signalsFilePath, timeMarksPath) {
  signals <- importFromTXT(signalsFilePath)
  sessionInfo <- getSessionInfo(timeMarksPath)
  
  session <- list()
  
  # Header
  
  session$`Header` <- signals$header
  
  # Task path to Task names
  
  session$`Task path` <- sessionInfo$`Task path`
  # session$`Task name` <- pathToNames(sessionInfo$`Task path`)
  
  # Times
  
  # session$Times <- data.frame(startTime = sessionInfo$`Start time`, endTime = sessionInfo$`End time`)
  times <- data.frame(startTime = sessionInfo$`Start time`, endTime = sessionInfo$`End time`)
  
  # Times to marks
  
  # session$`Marks` <- timesToMarks(signals$header$outputRate, signals$header$date, session$Times)
  marks <- timesToMarks(signals$header$outputRate, signals$header$date, times)
  
  session$`Description` <- data.frame(taskName = pathToNames(sessionInfo$`Task path`),
                                      startTime = format(times$startTime, '%H:%M:%S'),
                                      endTime = format(times$endTime, '%H:%M:%S'),
                                      startMark = marks$startMark,
                                      endMark = marks$endMark)
  
  # Signals
  session$`Signals` <- resampleSignals(signals$channels)
  
  session
}