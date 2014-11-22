cfBuild <- function (inputData, inputClass, ...) UseMethod("cfBuild") 

cfBuild.default <- function(inputData, inputClass, bootNum = 100, ensNum = 100, parallel = TRUE, cpus = NULL, type = "SOCK", socketHosts = NULL, ...) {
  
  if(.init(inputData, inputClass)) {
    inputData   = as.matrix(as.data.frame(inputData))
    inputClass  = as.factor(as.matrix(inputClass))
    
    svmObj = .snowRBF(inputData, inputClass, bootNum, ensNum, parallel, cpus, type, socketHosts)
    
    return(svmObj)
  }
}
