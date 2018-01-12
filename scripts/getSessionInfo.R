# openTestFile <- function(path, name, file) {
#     testInfoFilePath = paste(path, name, '-', file, '.txt', sep="");
#     file <- readLines(testInfoFilePath);
#     file
# }

openTestFile <- function(filepath) {
    file <- readLines(filepath, encoding = 'latin1');
    file
}


getTaskDataFrame <- function(file) {
  file <- file[-1]
  file <- strsplit(file, ',')
  file <- data.frame(matrix(unlist(file), length(file), 3, T))
  names(file) <- c('Task path', 'Start time', 'End time')
  file$`Task path` <- as.character(file$`Task path`)
  file$`Start time` <- as.POSIXct(file$`Start time`, format ='%H:%M:%S')
  file$`End time` <- as.POSIXct(file$`End time`, format ='%H:%M:%S')
  file
}


# getSessionInfo <- function(path, timeMarks, signalsFile) {
#   data <- openTestFile(path, timeMarks, signalsFile);
#   data <- getTaskDataFrame(data)
#   
#   data
# }

getSessionInfo <- function(filepath) {
  data <- openTestFile(filepath);
  data <- getTaskDataFrame(data)
  
  data
}