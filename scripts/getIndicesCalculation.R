# Get values of references of the closer baseline of the startMark
getValuesOfRefOfThisStartMark <- function(valuesOfReference, startMark)
{
  minDiff   <- 100000000
  minIndex  <- 100000000
  for (index in 1:length(valuesOfReference))
  {
    basalMark <- valuesOfReference[[index]]$startMark
    if (basalMark < startMark)
    {
      diff <- abs(basalMark - startMark)
      if (diff < minDiff)
      {
        minDiff <- diff
        minIndex <- index
      }
    }
  }
  return(valuesOfReference[[minIndex]]$values)
}



# Return Zscore normalization above 'x' param
zscore <- function(x,mean,std)
{
  return ((x-mean)/std)
}

# Function which calculates the values of the indices of all tasks.
# Its params are:
#  -> rightData: right data of tasks
#  -> leftData: left data of tasks
#  -> valuesOfReference: values of reference of indices, for each baseline task
#
# Return a list with as many fields as there are tasks. 
#  Each task is a list with following fields:
#   -> AI: value of Attention Index
#   -> MI: value of Memorization Index
#   -> AW: value of Approach Widthdrawal Index
#   -> EI: value of Engagement Index
#
#  All of them are lists with the following fields:
#   -> ZSp2p: point to point normalized data, of the index.
#   -> value: value of the index. 
getIndicesCalculation <- function(rightData, leftData, valuesOfReference)
{
  # Result of function
  indicesCalculation <- list()
  
  # Get tasks names
  tasks <- names(rightData)
  # For each task
  for (task in tasks)
  {
    # Get bands of task
    # Right bands
    rightAlpha <- rightData[[task]]$alpha
    rightBeta  <- rightData[[task]]$beta
    rightTheta <- rightData[[task]]$theta
    # Left bands
    leftAlpha <- leftData[[task]]$alpha
    leftBeta  <- leftData[[task]]$beta
    leftTheta <- leftData[[task]]$theta
    
    # Get startMark of task
    startMark <- rightData[[task]]$startMark
    
    # Get values of reference for this task:
    valOfRef <- getValuesOfRefOfThisStartMark(valuesOfReference, startMark)
    AIref <- valOfRef$AI
    MIref <- valOfRef$MI
    AWref <- valOfRef$AW
    EIref <- valOfRef$EI
    
    ## AI index calculation
    tempAI <- getAttentionIndex(rightData = rightAlpha, leftData = leftAlpha)
    AIzs   <- zscore(tempAI, mean = AIref$mean, std = AIref$std)
    AI     <- mean(AIzs)
    
    ## MI index calculation
    tempMI <- getMemorizationIndex(rightData = rightTheta, leftData = leftTheta)
    MIzs   <- zscore(tempMI, mean = MIref$mean, std = MIref$std)
    MI     <- mean(MIzs)
    
    ## AW index calculation
    tempAW <- getApproachWidthdrawalIndex(rightData = rightAlpha, leftData = leftAlpha)
    AWzs   <- zscore(tempAW, mean = AWref$mean, std = AWref$std)
    AW     <- mean(AWzs)
    
    ## EI index calculation
    tempEI <- getEngagementIndex(rightData = rightData[[task]], leftData = leftData[[task]])
    EIzs   <- zscore(tempEI, mean = EIref$mean, std = EIref$std)
    EI     <- mean(EIzs)
    
    indicesCalculation[[task]] <- list(
      AI = list(
        ZSp2p = AIzs,
        value = AI
      ),
      MI = list(
        ZSp2p = MIzs,
        value = MI
      ),
      AW = list(
        ZSp2p = AWzs,
        value = AW
      ),
      EI = list(
        ZSp2p = EIzs,
        value = EI
      )
    )
  }
  
  return(indicesCalculation)
}