# Functions for permutation


runPermutation <- function (inputData, inputClass, bootNum = 100, ensembleNum = 100, permNum=100, runParallel=TRUE, cpus=NULL, type = "SOCK", socketHosts = NULL, progressBar = TRUE)
{
  permMatr = .getPermMatr(inputClass, permNum)
  
  allPermObj = list()
  meanVal = c()
  
  permObj <- .snowRBFperm(inputData, permMatr, bootNum, ensembleNum, permNum, runParallel, cpus, type, socketHosts, progressBar)
  
  return (permObj)
}


.getPermMatr <- function (classVec, permNum)
{
  permMatr = c()
  
  for (permIt in 1:permNum)
  {
    set.seed(permIt)
    randomClasses = as.vector(sample(classVec))
    permMatr = rbind(permMatr, randomClasses)
  }
  
  permMatr = as.data.frame(permMatr)
  rownames(permMatr) = NULL
  
  return (permMatr)
}



# Run parallel scripts using the snow/snowfall package in R

.snowRBFperm <- function(inputData, permMatr, bootNum, ensembleNum, permNum, runParallel, cpus, type, socketHosts, progressBar)
{
  require(snowfall, quietly = TRUE)
  
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
        sfExport("bootNum", local=TRUE)
        
        permList = totalRunTime = avgTestAcc = c()
        
        if (progressBar == TRUE) {
          pb <- txtProgressBar(min = 0, max = permNum, style = 3)
        }
        
        for (k in 1:permNum) { 
          
          if (progressBar == TRUE) { Sys.sleep(0.1) }
          inputClass = as.factor(as.matrix(permMatr[k,]))
          sfExport("inputClass", local=TRUE)
          
          execTime = system.time(parComplexRes <- sfLapply(1:ensembleNum, .runRadialComplex))
          permList[[k]] = parComplexRes
          avgTestAcc = c(avgTestAcc, mean(sapply(parComplexRes,"[[",1)))
          totalRunTime = rbind(totalRunTime, execTime)
          
          if (progressBar == TRUE) { setTxtProgressBar(pb, k) }
        }
        
        rownames(totalRunTime) = NULL
        
        if (progressBar == TRUE) { close(pb) }
        
        return(list(avgTestAcc = avgTestAcc,
                      execTime   = totalRunTime[,1:3],
                      permList   = permList))
        
        sfStop()}, finally=sfStop())
    } else {
      warning ("Please load the packages: boot, neldermead and e1071")
    }
  }
  else 
  {
    warning ("The Rmpi package has not been loaded. Please try again.")
  }
}


# 
# .execPerm <- function (permMatr, permNum)
# {
#   totalRunTime = c()
#   
# #   for (permIter in 1:permNum)
#   {
#     message("----------------------------------------------------")
#     message(permIter)
#     classVector = permMatr[permIter,]
#     
#     parRunTime = system.time(parComplexRes <- mpi.parLapply(1:100, runRadialComplexParallel, testMatr, classVec))
#     classAcc = c(classAcc, mean(sapply(parComplexRes,"[[",1)))
#     totalRunTime = c(totalRunTime, parRunTime)
#     message(mean(sapply(parComplexRes,"[[",1)))
#     dput(parComplexRes, paste("perm", permIter, sep=""))
#     
#     
#     dput(classAcc, "classAcc")
#     dput(totalRunTime, "totalRunTime")
#     save.image("workspace.RData")
#     write.table(as.matrix(classAcc), file="classAcc.csv", sep=",", append=FALSE, row.names=FALSE, col.names=FALSE)
#   }
#   
#   write.table(as.matrix(classAcc), file="classAcc.csv", sep=",", append=FALSE, row.names=FALSE, col.names=FALSE)
# }


