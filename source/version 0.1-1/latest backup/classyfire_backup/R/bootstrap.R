# Private functions for bootstrapping

.bootIndx <- function(trainCl, indices) {
  testIndx = trainIndx = c()
  
  bootTrainCl <- trainCl[indices]
  bootTestCl  <- trainCl[-indices]
  
  if (is.element(0, table(bootTrainCl)) || is.element(0, table(bootTestCl))) {   
    classLevels <- levels(trainCl)
    classLength <- nlevels(trainCl)
    
    for (j in 1:classLength) {
      classIndx <- which(trainCl == classLevels[j])
      randIndx  <- sample(classIndx, 2, replace=FALSE)
      testIndx  <- c(testIndx,  randIndx[1])
      trainIndx <- c(trainIndx, randIndx[2])
    }
    indices <- c(trainIndx, sample(setdiff(1:length(trainCl), testIndx), length(trainCl), replace=TRUE))
  }
  return(indices) 
} 
