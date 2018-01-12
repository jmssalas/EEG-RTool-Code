convertMatrixToList <- function(matrix) 
{
  l <- list()
  for (signal in colnames(matrix))
  {
    l[[signal]] <- matrix[,signal]
  }
  return(l)
}