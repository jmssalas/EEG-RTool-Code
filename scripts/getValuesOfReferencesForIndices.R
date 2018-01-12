# Function which calculates the values of references for indices calculation.
# Its params are:
#  -> rightBasalData: right data of baseline task
#  -> leftBasalData: left data of baseline task
#
# Return a list with following fields:
#   -> AI: value of references for Attention Index
#   -> MI: value of references for Memorization Index
#   -> AW: value of references for Approach Widthdrawal Index
#   -> EI: value of references for Engagement Index
#
#  All of them are lists with the following fields:
#   -> p2p: point to point index data.
#   -> mean: mean of index. 
#   -> std: standard deviation of index.
getValuesOfReferencesForIndices <- function(rightBasalData, leftBasalData)
{
  rightAlpha <- rightBasalData$alpha
  rightBeta  <- rightBasalData$beta
  rightTheta <- rightBasalData$theta
  
  leftAlpha <- leftBasalData$alpha
  leftBeta  <- leftBasalData$beta
  leftTheta <- leftBasalData$theta
  
  
  ## AI index calculation
  AI     <- getAttentionIndex(rightData = rightAlpha, leftData = leftAlpha)
  meanAI <- mean(AI)
  stdAI  <- sd(AI)
  
  ## MI index calculation
  MI     <- getMemorizationIndex(rightData = rightTheta, leftData = leftTheta)
  meanMI <- mean(MI)
  stdMI  <- sd(MI)
  
  ## AW index calculation
  AW     <- getApproachWidthdrawalIndex(rightData = rightAlpha, leftData = leftAlpha)
  meanAW <- mean(AW)
  stdAW  <- sd(AW)
  
  ## EI index calculation
  EI     <- getEngagementIndex(rightData = rightBasalData, leftData = leftBasalData)
  meanEI <- mean(EI)
  stdEI  <- sd(EI)
  
  return(
    list(
      AI = list(
        p2p  = AI,
        mean = meanAI,
        std  = stdAI
      ),
      MI = list(
        p2p  = MI,
        mean = meanMI,
        std  = stdMI
      ),
      AW = list(
        p2p  = AW,
        mean = meanAW,
        std  = stdAW
      ),
      EI = list(
        p2p  = EI,
        mean = meanEI,
        std  = stdEI
      )
    )
  )
}