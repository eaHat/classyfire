
.loadLibraries <- function (runParallel = FALSE, slavesNum = NULL)
{
  message("Loading libraries\n")
  require(e1071, quietly = TRUE)
  require(boot, quietly = TRUE, warn.conflicts = FALSE)
  require(neldermead, quietly = TRUE)
  
  if (runParallel == TRUE)
  {
    require(Rmpi, quietly = TRUE)
    
    if ("package:Rmpi" %in% search())
    {
      if (missing(slavesNum) || is.null(slavesNum)) 
      {
        slavesNum = mpi.universe.size()
      }
      
      mpi.spawn.Rslaves(nslaves = slavesNum, needlog = FALSE)
      
      if (mpi.comm.size() != (slavesNum+1)) {
        print(paste("Please initialize an MPI cluster of at least", slavesNum," processors. Then try again.", sep=""))
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
          .Call("mpi_finalize")
        }
      }
      
      sz <- mpi.comm.size()
      
      if (isTRUE(unique(c("package:boot", "package:neldermead","package:e1071") %in% search())))
      {
        mpi.bcast.cmd(library(neldermead))
        mpi.bcast.cmd(library(e1071))
        mpi.bcast.cmd(library(boot))
        
        # More to write here ........... 
        
        #warning ("Closing all slaves.")
        #mpi.close.Rslaves() 
        
      } else 
      {
        mpi.close.Rslaves()  
      }
     
    } 
    else 
    {
      warning ("The Rmpi package has not been loaded. Please try again.")
    }
  }
}
