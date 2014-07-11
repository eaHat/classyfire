# Statistics 

testAcc <- function(ens) {
  vec =  sapply(ens,"[[",1)
  return (round(vec, digits = 2))
}

avgTestAcc <- function(ens) {
  return(round(mean(sapply(ens,"[[",1)), digits=2))
}


trainAcc <- function(ens) {
  vec =  sapply(ens,"[[",2)
  return (round(vec, digits = 2))
}

avgTrainAcc <- function(ens) {
  return(round(mean(sapply(ens,"[[",2)), digits=2))
}

confMatr <- function(iter, ens) { 
  confTable = ens[[iter]]$conf_matrix
  return(confTable)
}

optimParams <- function(ens) {
  gamma =  sapply(ens,"[[", 14)
  cost  =  sapply(ens,"[[", 15)
  
  matr = cbind(gamma, cost)
  colnames(matr) = c("optim Gamma", "optim Cost")
  
  return (matr)
}

overallClassPred <- function(ens) {
  confMatr = apply(sapply(ens,"[[",3), 1, "sum")
  tbc = matrix(confMatr, nrow = nrow(ens[[1]]$conf_matrix), ncol = ncol(ens[[1]]$conf_matrix))
  propTable = round(prop.table(tbc, 1)*100)
  colnames(propTable) = colnames(ens[[1]]$conf_matrix)
  rownames(propTable) = rownames(ens[[1]]$conf_matrix)
  return(propTable)
  #diag(round(prop.table(tbc, 1)*100))
}

fiveNumSummary <- function(permObj) {
  minVal = min(permObj$avgTestAcc)
  maxVal = max(permObj$avgTestAcc)
  medianVal = median(permObj$avgTestAcc)
  quantiles = quantile(permObj$avgTestAcc)
  
  fiveNumSum = c()
  
  fiveNumSum$minimum = minVal
  
  return (list(minimum = minVal,
               lowerQ  = quantiles[2],
               median  = medianVal,
               upperQ  = quantiles[4],
               maximum = maxVal))
}
