# Perform init checks

.initCheck <-function(inputData, inputClass)
{
  if (missing(inputData) || is.null(inputData)) 
  {
    warning("You must provide an input data matrix.")
    
  } else if (missing(inputClass) || is.null(inputClass)) 
  {
    warning("You must provide a class vector.")
    
  } else 
  {
    inputMatrix = as.matrix(as.data.frame(inputData))
    inputClass  = as.factor(as.matrix(inputClass))
    
    if (length(inputClass) != nrow(inputMatrix))
    {
      warning("You must provide a class vector equal in length to the number of rows in the data matrix.")
      
    } else 
    {
      return (TRUE)
    }
  }
}