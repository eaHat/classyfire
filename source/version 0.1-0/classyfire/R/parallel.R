.snowRBF <- function(inputData, inputClass, bootNum, ensNum, parallel, cpus, type, socketHosts) {
  tryCatch({
    sfInit( parallel = parallel, cpus = cpus, type = type, socketHosts = socketHosts)
    
    # Send the libraries
    sfLibrary("neldermead", character.only=TRUE)
    sfLibrary("e1071",      character.only=TRUE)
    sfLibrary("boot",       character.only=TRUE)
    
    parComplexRes <- sfLapply(1:ensNum, .boxRadial, inputData, inputClass, bootNum)
    
    return(parComplexRes)
    
    message(paste("Analysis Complete in", parComplexRes$runTime, "seconds" , sep=" "))
    
    sfStop()
  }, finally=sfStop())
}