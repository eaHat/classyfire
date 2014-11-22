# ************************************************************************
# Perform initial checks on input data and input class
# Throw warning messages in case this step fails
# ************************************************************************


.initCheck <-function(inputData, inputClass) {
  
  # Throw warnings and exit if inputData and inputClass are missing or null
  if (missing(inputData) || is.null(inputData)) {
    stop("You must provide an input data matrix.")
  } 
  else if (missing(inputClass) || is.null(inputClass)) {
    stop("You must provide a class vector.") 
  } 
  else {
    # Convert the input datasets into the right format:  
    # inputData into a matrix and inputClass into a factor
    
    inputMatrix <- as.matrix(as.data.frame(inputData))
    inputClass  <- as.factor(as.matrix(inputClass))
    
    # Stop and throw a warning if the length of the input vector  
    # is not the same as the num of rows in the input matrix
    if (length(inputClass) != nrow(inputMatrix)) {
      stop("You must provide a class vector equal in length to the number of rows in the data matrix.")
    } else {
      return (TRUE)
    }
  }
}