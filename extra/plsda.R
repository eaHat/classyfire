
.runPLSDA <- function(seed)
{ 
  runTime = system.time({
    
    testSamples = complexRes = c()
    
    # seed - provides the iteration number of the ensemble
    # plus, it is used by the Rmpi/snowfall packages for parallel programming
    
    set.seed(seed)
    
    testSamples = .buildTestSamples(seed, inputClass)
    
    trainData  = inputData[-testSamples,]
    testData   = inputData[testSamples,]
    trainClass = inputClass[-testSamples]
    testClass  = inputClass[testSamples]
    rownames(trainData) = NULL
    
#     b = boot(data = trainData, statistic = .bootIndx, R = bootNum)
#     bootRes = b$t
    
    #   for (i in 1:nrow(bootRes))
    #   {
    #     indices = bootRes[i,]
    #     indices = .bootControl(trainClass = trainClass, indices = indices)
    #     bootRes[i,] = indices 
    #   }
    
    
    bootTime = system.time(b <- boot(data = trainData, statistic = .bootPLSDA, R = bootNum, trainClass = trainClass))
    bootRes = t(b$t)
    
    avgRes = as.matrix(apply(bootRes, 1, mean))
    optLV = which.max(avgRes)
    
    plsdaModel = pls.lda(trainData, trainClass, testData, ncomp=optLV, nruncv=0)
    predTest  = plsdaModel$predclass
    names(predTest) = testSamplesNames
    
    confMatr = table(pred= predTest, true= testClass)
    trueTest  = length(which(predTest == testClass, arr.ind=TRUE))
    acc = (trueTest*100)/length(testClass)
    
    
    missNames = rownames(testData)[which((predTest != testClass), arr.ind=TRUE)]
    accNames = rownames(testData)[which((predTest == testClass), arr.ind=TRUE)]
    
    return(list(acc = acc, 
                optLV = optLV,
                confMatr = confMatr,
                missNames = missNames,
                accNames = accNames,
                testSamples = testSamples, 
                predTest = predTest,
                bootTime = bootTime,
                avgBootRes = as.vector(avgRes), 
                bootObj = b))
}


.bootPLSDA <- function(data, trainClass, indices) 
{ 
  cAcc = c()
  
  maxLV = ncol(data)
  
  if (maxLV > 30)
  {
    maxLV = 30 
  }
  
  indices = bootControl(trainClass = trainClass, indices = indices)
  
  bootTrain = data[indices,]
  bootTest  = data[-indices,]
  
  bootTrainClass = trainClass[indices]
  bootTestClass  = trainClass[-indices]
  
  
  if (nrow(bootTest) > 0)
  {
    for (lv in 1:maxLV)
    {
      plsdaModel = NULL
      plsdaModel = pls.lda(bootTrain, bootTrainClass, bootTest, ncomp=lv, nruncv=0)
      predClass  = plsdaModel$predclass
      
      trueTest  = length(which(predClass == bootTestClass, arr.ind=TRUE))
      classified = (trueTest*100)/length(bootTestClass)
      cAcc = c(cAcc, classified)
    }  	
  }
  if (length(cAcc) == 0)
  {
    cAcc = seq(from = 1, to = maxLV, by = 1)
  }
  
  return(cAcc) 
} 
