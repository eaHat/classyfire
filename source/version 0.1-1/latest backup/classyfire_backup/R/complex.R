### Where k is the seed number and classVec the class vector to be used for the test samples

.boxRadial <- function(seed, inputData, inputClass, bootNum) {
  runTime = system.time({
    testSamples = complexRes = c()
     
    # seed - provides the iteration number of the ensemble and it is used by the Rmpi/snowfall packages for parallel programming
    set.seed(seed)
    
    testSamples = .randSamples(seed, inputClass)
    trainData   = inputData[-testSamples,]
    testData    = inputData[testSamples,]
    trainClass  = inputClass[-testSamples]
    testClass   = inputClass[testSamples]
    
    b = boot(data = trainClass, statistic = .bootIndx, R = bootNum)
    bootRes = b$t
    
    # Random initial points
    x1 = sample(-14:4, 1)
    x2 = sample(-4:11, 1)
    
    # Train/optim with the complex algorithm 
    complexRes <- .complexFunc(iterNum = seed, initPoints = c(x1,x2), bootRes = bootRes, trainData = trainData, trainClass = trainClass)
      
    svmMod    = svm(trainData, trainClass, type="C-classification", kernel="radial",  gamma= 2^(complexRes$xopt[1]), cost= 2^(complexRes$xopt[2]))
    predTest  = predict(svmMod, testData)
    predTrain = predict(svmMod, trainData)
    
    trueTest  = length(which(predTest == testClass, arr.ind=TRUE))
    accTest   = (trueTest*100)/length(testClass)
    
    trueTrain = length(which(predTrain == trainClass, arr.ind=TRUE))
    accTrain  = (trueTrain*100)/length(trainClass)
    
    missNames = rownames(testData)[which((predTest != testClass), arr.ind=TRUE)]
    accNames  = rownames(testData)[which((predTest == testClass), arr.ind=TRUE)]
  
    Predicted = predTest
    Test_Class= testClass
  })
  
  return(list(testAcc     = accTest,
              trainAcc    = accTrain,
              optGamma    = complexRes$xopt[1], 
              optCost     = complexRes$xopt[2],
              x1=x1,
              x2=x2,
              runTime     = runTime,
              confMatr    = table(Predicted, Test_Class),
              propTable   = round(table(Predicted, Test_Class)*100),
              predClass   = predTest,
              testClass   = testClass,
              missNames   = missNames,
              accNames    = accNames, 
              testSamples = testSamples, 
              svmModel    = svmMod))
}


.randSamples <- function(iter, classVec) {
  
  set.seed(iter)
  
  # Empty vector to store the test samples
  testSamples = c()
  
  # Count the different levels in the classVec
  classVec    = as.factor(as.matrix(classVec))  
  classLevels = levels(classVec)
  classLength = length(classLevels)
  
  # Store 1/3 of the samples from each class in the testSamples vector
  for (j in 1:classLength) {
    indx = which(classVec == classLevels[j])
    testSamples = c(testSamples, sample(indx, round(length(indx)/3), replace=FALSE))
  }
  
  return(testSamples)
}


# Cost function

.radialSVM <- function(x= NULL, index = NULL, fmsfundata = NULL) {
  cErr <- c()
	bootIndices <- fmsfundata$bootRes
	trainData   <- fmsfundata$trainData
	trainClass  <- fmsfundata$trainClass
	
	for (i in 1:nrow(bootIndices)) {
		indices      <- bootIndices[i,]
		bTrainData   <- trainData[indices,]
		bTestData    <- trainData[-indices,]
		bTrainClass  <- trainClass[indices]
		bTestClass   <- trainClass[-indices]
    
		rownames(bTrainData) <- NULL
		
		svmMod    <- svm(bTrainData, bTrainClass, type="C-classification", kernel="radial",  gamma= 2^(x[1]), cost= 2^(x[2]))
    predClass <- predict(svmMod, bTestData)
		falseTest <- length(which(as.vector(predClass) != as.vector(bTestClass), arr.ind=TRUE))
		missclass <- (falseTest*100)/length(bTestClass)
		cErr      <- c(cErr, missclass)
	}
	avgSVMerr <- round(mean(cErr), digits=2)
	
	return(list( f = avgSVMerr, g = c(), c = c(), gc = c(), index = index, this = list(costfargument = fmsfundata)))
}


# Box constrained simplex

.complexFunc <- function(iterNum = NULL, initPoints = NULL, bootRes = NULL, trainData = NULL, trainClass = NULL) {
	set.seed(iterNum)
	x0 <- transpose(c(initPoints[1], initPoints[2]))
	
	fmsfundata <- structure(list(bootRes=bootRes, trainData=trainData, trainClass=trainClass), class='optimbase.functionargs')
	
	nm <- neldermead()
	nm <- neldermead.set(nm, 'numberofvariables', 2)
	nm <- neldermead.set(nm, 'costfargument', fmsfundata)
	nm <- neldermead.set(nm, 'function', .radialSVM)
	nm <- neldermead.set(nm, 'x0', x0)
	nm <- neldermead.set(nm, 'verbose', FALSE)
	nm <- neldermead.set(nm, 'storehistory', TRUE)
	nm <- neldermead.set(nm, 'verbosetermination', FALSE)
	nm <- neldermead.set(nm, 'method', "box")
	nm <- neldermead.set(nm, 'boundsmin', c(-15,-5))
	nm <- neldermead.set(nm, 'boundsmax', c(5,13))
 	nm <- neldermead.set(nm, 'boxnbpoints', 3)
	nm <- neldermead.set(nm, 'simplex0method', "randbounds")
	nm <- neldermead.set(nm, 'scalingsimplex0', "tocenter")
	nm <- neldermead.search(nm)
	
	return(list(xopt = transpose(neldermead.get(nm, 'xopt'))))
}

