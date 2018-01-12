getAttentionIndex <- function(rightData, leftData)
{
  rightGFP <- rightData^2 
  leftGFP  <- leftData^2
  AI <- -((rightGFP + leftGFP) / 2)
  
  return(AI)
}

getMemorizationIndex <- function(rightData, leftData)
{
  rightGFP <- rightData^2 
  leftGFP  <- leftData^2
  MI <- ((rightGFP + leftGFP) / 2)
  
  return(MI)
}

getEngagementIndex <- function(rightData, leftData)
{
  rightAlpha <- rightData$alpha
  rightBeta  <- rightData$beta
  rightTheta <- rightData$theta
  
  leftAlpha <- leftData$alpha
  leftBeta  <- leftData$beta
  leftTheta <- leftData$theta
  
  alphaGFP <- (rightAlpha^2 + leftAlpha^2)/2 
  betaGFP  <- (rightBeta^2 + leftBeta^2)/2
  thetaGFP <- (rightTheta^2 + leftTheta^2)/2
  EI <- betaGFP / (alphaGFP + thetaGFP)
  
  return(EI)
}

getApproachWidthdrawalIndex <- function(rightData, leftData)
{
  rightGFP <- rightData^2 
  leftGFP  <- leftData^2
  AWI <- rightGFP - leftGFP
  
  return(AWI)
}