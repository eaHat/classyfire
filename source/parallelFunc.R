.snowRBF <- function(inputData, inputClass, bootNum, ensNum, parallel, cpus, type, socketHosts) {
  tryCatch({
    sfInit( parallel = parallel, cpus = cpus, type = type, socketHosts = socketHosts)
    
    # Send the libraries
    sfLibrary(neldermead)
    sfLibrary(e1071)
    sfLibrary(boot)
      
    # Send the variables
    sfExport("inputData",  local=TRUE)
    sfExport("inputClass", local=TRUE)
    sfExport("bootNum",    local=TRUE)
    
#     sfExport(".randSamples", local=TRUE)
#     sfExport(".radialSVM",   local=TRUE)
#     sfExport(".complexFunc", local=TRUE)
#     sfExport(".bootIndx",    local=TRUE)
    
    parComplexRes <- sfLapply(1:ensNum, .boxRadial)
    return(parComplexRes)
    message(paste("Analysis Complete in", parComplexRes$runTime, "seconds" , sep=" "))
    sfStop()
  }, finally=sfStop())
}