library(shiny)
library(signal)
library(DT)
library(seewave)
library(SDMTools) # For Center Of Gravity calculation
library(dygraphs)
library(fftw)

# Function for source all files into 'directory' folder
sourceDirectory <- function(directory) 
{
  pathnames <- list.files(pattern='[.]R$', path=directory, full.names=TRUE)
  sapply(pathnames, FUN=source)
}

sourceDirectory('scripts')
sourceDirectory('server/modules')
sourceDirectory('server')

# runApp('server/', host = '123.123.123.123', port = 1234) # For any server
runApp('server/') # For localhost