# ************************************************************************
# Function for parallelisation of the classification ensemble 
# ************************************************************************

.snowRBF <- function(inputData, inputClass, bootNum, ensNum, parallel, cpus, type, socketHosts) {
  tryCatch({
    # Initialisation using given specs from user
    sfInit(parallel=parallel, cpus=cpus, type=type, socketHosts=socketHosts)
    
    # Send the libraries
    sfLibrary("neldermead", character.only=TRUE)
    sfLibrary("e1071",      character.only=TRUE)
    sfLibrary("boot",       character.only=TRUE)
    
    # Parallelise the .boxRadial function, ensNum times
    # Pass additional arguments: inputData, inputClass, bootNum
    parRBFobj <- sfLapply(1:ensNum, .boxRadial, inputData, inputClass, bootNum)
    
    return(parRBFobj)
    
    # Terminate 
    sfStop()
  }, finally=sfStop())
}
