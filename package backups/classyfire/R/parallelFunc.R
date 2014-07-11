
# Run parallel scripts using the snow/snowfall package in R

.snowRBF <- function(inputData, inputClass, bootNum, ensembleNum, runParallel, cpus, type, socketHosts)
{
  if ("package:snowfall" %in% search()) {
    
    if (isTRUE(unique(c("package:boot", "package:neldermead","package:e1071") %in% search()))) {
      
      tryCatch({
        sfInit( parallel = runParallel, cpus = cpus, type = type, socketHosts = socketHosts)
        
        # Send the libraries
          
        sfLibrary(neldermead)
        sfLibrary(e1071)
        sfLibrary(boot)
          
        # Send the variables
          
        sfExport("inputData", local=TRUE)
        sfExport("inputClass", local=TRUE)
        sfExport("bootNum", local=TRUE)
        
        parComplexRes <- sfLapply(1:ensembleNum, .runRadialComplex)
        return(parComplexRes)
        
        sfStop()}, finally=sfStop())
    } else {
      warning ("Please load the packages: boot, neldermead and e1071")
    }
  }
  else 
  {
    warning ("The snowfall package has not been loaded. Please try again.")
  }
}