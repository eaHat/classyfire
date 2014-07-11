
ensRBF <- function (inputData, inputClass, ...) UseMethod("ensRBF") 

ensRBF.default <- function(inputData, inputClass, bootNum = 100, ensembleNum = 100, runParallel = TRUE, cpus = NULL, type = "SOCK", socketHosts = NULL) {
  
  if(.initCheck(inputData, inputClass)) {
    
    inputData = as.matrix(as.data.frame(inputData))
    inputClass  = as.factor(as.matrix(inputClass))
    
    svmObj = .snowRBF(inputData, inputClass, bootNum, ensembleNum, runParallel, cpus, type, socketHosts)
    
    return(svmObj)
  }
}

# ensPLDA <- function(inputData, inputClass, bootNum = 100, ensembleNum = 100, runParallel = FALSE, cpus = NULL, type = "SOCK", socketHosts = NULL) {
#   
#   if(.initCheck(inputData, inputClass)) {
#     
#     inputData = as.matrix(as.data.frame(inputData))
#     inputClass  = as.factor(as.matrix(inputClass))
#     
#     plsdaObj = .snowPLSDA(inputData, inputClass, bootNum, ensembleNum, runParallel, cpus, type, socketHosts)
#     
#     return(plsdaObj)
#     
#   }
# }
  