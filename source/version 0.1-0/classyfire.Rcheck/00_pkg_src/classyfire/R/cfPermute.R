# Functions for permutation

cfPermute <- function (inputData, inputClass, bootNum = 100, ensNum = 100, permNum=100, parallel=TRUE, cpus=NULL, type = "SOCK", socketHosts = NULL, progressBar = TRUE) {
  permMatr = .getPermMatr(inputClass, permNum)
  
  allPermObj = list()
  meanVal = c()
  
  permObj <- .snowRBFperm(inputData, permMatr, bootNum, ensNum, permNum, parallel, cpus, type, socketHosts, progressBar)
  
  return (permObj)
}


.getPermMatr <- function (classVec, permNum) {
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

.snowRBFperm <- function(inputData, permMatr, bootNum, ensNum, permNum, parallel, cpus, type, socketHosts, progressBar) {
  tryCatch({
    sfInit( parallel = parallel, cpus = cpus, type = type, socketHosts = socketHosts)
    
    # Send the libraries
    sfLibrary("neldermead", character.only=TRUE)
    sfLibrary("e1071",      character.only=TRUE)
    sfLibrary("boot",       character.only=TRUE)
    
    permList = totalRunTime = avgTestAcc = c()
    
    if (progressBar == TRUE) {
      pb <- txtProgressBar(min = 0, max = permNum, style = 3)
    }
    
    for (k in 1:permNum) { 
      
      if (progressBar == TRUE) { Sys.sleep(0.1) }
      inputClass = as.factor(as.matrix(permMatr[k,]))
      
      execTime = system.time(parComplexRes <- sfLapply(1:ensNum, .boxRadial, inputData, inputClass, bootNum))
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
    
    sfStop()
  }, finally=sfStop())
}