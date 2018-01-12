convertListToMatrix <- function(list)
{
  df <- data.frame(list)
  colnames(df) <- names(list)
  return(df)
}