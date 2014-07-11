# Parallel Implementation

.rmpiRBF <- function (inputData, inputClass, ...) UseMethod("rmpiRBF") 

.rmpiRBF.default <-function(inputData, inputClass, bootNum = 100, ensembleNum = 100, cpus = NULL)
{
  if(.initCheck(inputData, inputClass)) {
    
    inputMatrix = as.matrix(as.data.frame(inputData))
    inputClass  = as.factor(as.matrix(inputClass))
  
    require(Rmpi, quietly = TRUE)
    
    if ("package:Rmpi" %in% search()) {
      
      if (missing(cpus) || is.null(cpus)) {
        cpus = mpi.universe.size()
      }
      
      mpi.spawn.Rslaves(nslaves = cpus, needlog = FALSE)
      
      if (mpi.comm.size() != (cpus+1)) {
        print(paste("Please initialize an MPI cluster of at least", cpus," processors. Then try again.", sep=""))
        mpi.quit()
      }
      
      # In case R exits unexpectedly, have it automatically clean up resources taken up by Rmpi (slaves, memory, etc...)
      
      .Last <- function(){
        if (is.loaded("mpi_initialize")){
          if (mpi.comm.size(1) > 0){
            print("Please use mpi.close.Rslaves() to close slaves.")
            mpi.close.Rslaves()
          }
          print("Please use mpi.quit() to quit R")
  #         .Call("mpi_finalize")
        }
      }
      
      sz <- mpi.comm.size()
      
      if (isTRUE(unique(c("package:boot", "package:neldermead","package:e1071") %in% search()))) {
        
        # Send the libraries
        
        mpi.bcast.cmd(library(neldermead))
        mpi.bcast.cmd(library(e1071))
        mpi.bcast.cmd(library(boot))
        
        # Send the variables 
        
        mpi.bcast.Robj2slave(inputMatrix)
        mpi.bcast.Robj2slave(inputClass)
        mpi.bcast.Robj2slave(ensembleNum)
        mpi.bcast.Robj2slave(bootNum)
        
        # Send the functions
        
        mpi.bcast.Robj2slave(.bootIndx)
        mpi.bcast.Robj2slave(.radialSVM)
        mpi.bcast.Robj2slave(.complexFunc)
        mpi.bcast.Robj2slave(.buildTestSamples)
        #mpi.bcast.Robj2slave(.bootControl)
        #mpi.bcast.Robj2slave(.runRadialComplex)
        
        parRunTimeComplex = system.time(parComplexRes <- mpi.parLapply(1:ensembleNum, runRadialComplex))
        #print(mean(sapply(parComplexRes,"[[",1)))
        #print(parRunTimeComplex)
        #print(parRunTimeComplex/60)
        #print(source("frequencies_boot_RBF.R"))
        
        message("Closing Slaves.")
        mpi.close.Rslaves()
        
        return(list(parRunTimeComplex = parRunTimeComplex,
                    parComplexRes     = parComplexRes))
        
      } else 
      {
        warning("The packages neldermead, e1071 and boot have not been loaded. Please, try again.")
        mpi.close.Rslaves()  
      }
    } 
    else 
    {
      warning ("The Rmpi package has not been loaded. Please try again.")
    }
  }
}
