# **************************************************************************************************************
# Functions for unit testing
# 
# Functions: 
#      test.initCheck: Checks for wrong input arguments, stops the functionality and throws an error  
#      
# **************************************************************************************************************

library('RUnit')
library('classyfire')

# Test data
data(iris)
irisClass <- iris[,5]
irisData  <- iris[,-5]
randClass <- c(2, rep(3, length(irisClass)-1))

ensObj  <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 5, ensNum = 2, parallel = FALSE)
permObj <- cfPermute(irisData, irisClass, bootNum = 5, ensNum = 2, permNum = 2, parallel = FALSE)



test.initCheck <- function() {
  checkException(.initCheck(), silent=TRUE)
  checkException(.initCheck(irisData), silent=TRUE)
  checkException(.initCheck(inputClass = inputClass), silent=TRUE)
  checkException(.initCheck(iris, irisClass),     silent=TRUE)
  checkException(.initCheck(irisData, irisData),  silent=TRUE)
  checkException(.initCheck(irisData, irisData),  silent=TRUE)
  checkException(.initCheck(irisData, randClass), silent=TRUE)
}

test.cfBuild <- function() {
  checkEquals("cfBuild",  class(ensObj)[2])
  checkEquals(13,         length(ensObj))
  checkEquals(95.1,       getAvgAcc(ensObj)$Test)
  checkEquals(96.97,      getAvgAcc(ensObj)$Train) 
  checkEquals(92.16,      ensObj$testAcc[1])
  checkEquals(96.97,      ensObj$trainAcc[1])
  checkEquals(TRUE,       any(attributes(ensObj)$names == "testAcc"))
  checkEquals(100,        getConfMatr(ensObj)[1,1])
  checkEquals(94,         getConfMatr(ensObj)[2,2])
  checkEquals(91,         getConfMatr(ensObj)[3,3])
}

test.cfPermute <- function() {
  checkEquals("cfPermute",  class(permObj)[2])
  checkEquals(4,            length(permObj))
  checkEquals(39.22,        permObj$avgAcc[1])
  checkEquals(2,            length(permObj$permList))
}

test.stats <- function() { 
  checkException(getAcc(),      silent=TRUE)
  checkException(getAvgAcc(),   silent=TRUE)
  checkException(getOptParam(), silent=TRUE)
  checkException(getConfMatr(), silent=TRUE)
  checkException(getPerm5Num(), silent=TRUE)
  
  checkException(getAcc(randClass),      silent=TRUE)
  checkException(getAvgAcc(randClass),   silent=TRUE)
  checkException(getOptParam(randClass), silent=TRUE)
  checkException(getConfMatr(randClass), silent=TRUE)
  checkException(getPerm5Num(randClass), silent=TRUE)
  
  checkEquals(2,         length(getAcc(ensObj)))
  checkEquals(2,         length(getAvgAcc(ensObj)))
  checkEquals(92.16,     getAcc(ensObj)$Test[1])
  checkEquals(96.97,     getAcc(ensObj)$Train[1])
  checkEquals(95.10,     getAvgAcc(ensObj)$Test)
  checkEquals(96.97,     getAvgAcc(ensObj)$Train)
  checkEquals("matrix",  class(getOptParam(ensObj)))
  checkEquals("table",   class(getConfMatr(ensObj)))
  checkEquals(9,         length(getConfMatr(ensObj)))
  checkEquals(5,         length(getPerm5Num(permObj)))
  checkEquals(33.33,     getPerm5Num(permObj)$minimum)
}

test.plots <- function() { 
  checkException(ggEnsTrend(),   silent=TRUE)
  checkException(ggEnsHist(),    silent=TRUE)
  checkException(ggClassPred(),  silent=TRUE)
  checkException(ggPermHist(),   silent=TRUE)
  
  checkException(ggEnsTrend(permObj),   silent=TRUE)
  checkException(ggEnsHist(permObj),    silent=TRUE)
  checkException(ggClassPred(permObj),  silent=TRUE)
  checkException(ggPermHist(ensObj),    silent=TRUE)
}

test.initCheck()
test.cfBuild()
test.cfPermute()
test.stats()
test.plots()
