# Perform init checks

.init <-function(inputData, inputClass) {
  if (missing(inputData) || is.null(inputData)) {
    stop("You must provide an input data matrix.")
  } 
  else if (missing(inputClass) || is.null(inputClass)) {
    stop("You must provide a class vector.") 
  } 
  else {
    inputMatrix = as.matrix(as.data.frame(inputData))
    inputClass  = as.factor(as.matrix(inputClass))
    
    if (length(inputClass) != nrow(inputMatrix)) {
      stop("You must provide a class vector equal in length to the number of rows in the data matrix.")
    } else {
      return (TRUE)
    }
  }
}