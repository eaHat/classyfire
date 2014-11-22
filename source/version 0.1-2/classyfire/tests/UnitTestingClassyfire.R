# Implementation for unit testing

library('RUnit')
library('classyfire')

data(iris)
irisClass <- iris[,5]
irisData  <- iris[,-5]
ens = c()

test.cfBuild <- function(){
  #checkException(ens <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 100, ensNum = 50, parallel = TRUE, cpus = 4, type = "SOCK"), 'Unable to load in parallel')
  ens <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 100, ensNum = 50, parallel = TRUE, cpus = 4, type = "SOCK")
  
  checkEquals(13,    length(ens))
  checkEquals(96.24, getAvgAcc(ens)$Test)
  checkEquals(98,    getAvgAcc(ens)$Train) 
  checkEquals(94.12, ens$testAcc[1])
  checkEquals(100,   ens$trainAcc[1])
  checkEquals(TRUE,  any(attributes(ens)$names == "testAcc"))
  checkEquals(100,   getConfMatr(ens)[1,1])
  checkEquals(97,    getConfMatr(ens)[2,2])
  checkEquals(92,    getConfMatr(ens)[3,3])
}

test.cfBuild()
