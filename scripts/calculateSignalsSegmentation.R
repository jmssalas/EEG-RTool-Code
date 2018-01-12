setVisited <- function(task, description)
{
  # For each row
  for (i in seq(1:nrow(description)))
  {
    # If 'task' has not been visited and the name is the same, return description updated
    if (!description[i,'visited'] && description[i,'taskName'] == task) 
    { 
      description[i,'visited'] <- TRUE
      return(description)
    }
  }
}

getRowOfTaskNotVisited <- function(description, task) 
{
  # For each row
  for (i in seq(1:nrow(description)))
  {
    # If 'task' has not been visited and the name is the same, return this row
    if (!description[i,'visited'] && description[i,'taskName'] == task) 
    { 
      # Return row
      return(i)
    }
  }
  # If it finishes here, 'task' has not been found.
  return(paste(task,'has not been found'))
}

getSignalsOfTask <- function(task, description, signals)
{
  result <- list() # List which contains the result
  
  row <- getRowOfTaskNotVisited(description, task) # Get row of task
  
  # Get start and end mark
  startMark <- description[row,4]
  endMark   <- description[row,5]
  
  # For each signal
  for (signal in names(signals))
  {
    # Add range of signal
    result[[signal]] <- signals[[signal]][startMark:endMark]
  }
  # Return result, startMark and newDescription
  return(list(result = result, startMark = startMark, newDescription = description))
}

# Function which calculates the signal segmentation by tasks.
# Its params are:
#  -> tasksPath: character vector which contains all tasks.
#  -> description: dataframe which contains the description of all tasks.
#  -> signals: signals to segment.
#
# Return a list with as many fields as there are signals in 'signal' param. 
#  Each signal is a list with three lists:
#   -> closed_eyes: Signal of '<closed_eyes>' task
#   -> baselines: List with as many fields as there are baselines tasks. The name of this fields
#      is: basal-startMark, where startMark is the number of its start mark in to the signal
#   -> tasks: List with as many fields as there are tasks (without closed_eyes and baselines tasks)
#
#   Each one contains the following fields:
#    -> startMark: Start mark into the signal
#    -> signal: Signal of tasks
calculateSignalsSegmentation <- function(tasksPath, description, signals)
{
  segmentation <- list()
  
  # description with a new column to know if a task has already been visited  
  # It has been done like this because it is posible that several 'baselines' have been
  # made to subjects.
  newDescription <- description
  newDescription[, 'visited'] <- FALSE
  
  # For each task:
  for (task in pathToNames(tasksPath))
  {
    # For each signal
    for (signal in names(signals))
    {
      # Get signals of task
      signalOfTask <- getSignalsOfTask(task, newDescription, signals)
      
      # Process 'closed_eyes' task 
      if (regexpr('closed_eyes', task) > 0)
      {
        # Store task's startMark
        segmentation[[signal]]$closed_eyes$startMark <- signalOfTask$startMark
        # Store task's range of signal
        segmentation[[signal]]$closed_eyes$signal <- signalOfTask$result[[signal]]
      }
      else 
      {
        # Process 'basal' task 
        if (regexpr('basal', task) > 0)
        {
          # Store basal into 'baselines' field
          baselineName <- paste('basal',signalOfTask$startMark,sep='-')
          segmentation[[signal]]$baselines[[baselineName]]$startMark <- signalOfTask$startMark
          segmentation[[signal]]$baselines[[baselineName]]$signal <- signalOfTask$result[[signal]]
        }
        else
        { # Process others tasks
          
          # Store task's startMark
          segmentation[[signal]]$tasks[[task]]$startMark <- signalOfTask$startMark
          # Store task's range of signal
          segmentation[[signal]]$tasks[[task]]$signal <- signalOfTask$result[[signal]]
        }
      }
    }
    # Set task like visited
    newDescription <- setVisited(task, newDescription)
  }
  return(segmentation)
}