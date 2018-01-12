getIndicesToCSV <- function(indices)
{
  csv <- list()
  for (task in names(indices))
  {
    indicesOfTask <- indices[[task]]
    for (indice in names(indicesOfTask))
    {
      csv[[indice]][[task]] <- indicesOfTask[[indice]]$value
    }
  }
  return(csv)
}