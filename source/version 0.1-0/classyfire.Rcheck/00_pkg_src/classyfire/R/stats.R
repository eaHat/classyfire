getAcc <- function(ensObj) {
  testAcc  <- round(sapply(ensObj, "[[", 1), digits=2)
  trainAcc <- round(sapply(ensObj, "[[", 2), digits=2)
  
  return (list(Test = testAcc, Train = trainAcc))
}

getAvgAcc <- function(ensObj) {
  avgTest  <- round(mean(sapply(ensObj, "[[", 1)), digits=2)
  avgTrain <- round(mean(sapply(ensObj, "[[", 2)), digits=2)
  
  return (list(Test = avgTest, Train = avgTrain))
}

getOptParam <- function(ensObj) {
  gamma <- sapply(ensObj,"[[", 3)
  cost  <- sapply(ensObj,"[[", 4)
  optMatr <- cbind(gamma, cost)
  colnames(optMatr) <- c("Optimum Gamma", "Optimum Cost")
  
  return (optMatr)
}

getConfMatr <- function(ensObj) {
  confMatr <- apply(sapply(ensObj,"[[", 6), 1, "sum")
  tbc <- matrix(confMatr, nrow = nrow(ensObj[[1]]$confMatr), ncol = ncol(ensObj[[1]]$confMatr))
  propTable <- round(prop.table(tbc, 1)*100)
  colnames(propTable) <- colnames(ensObj[[1]]$confMatr)
  rownames(propTable) <- rownames(ensObj[[1]]$confMatr)
  
  return(propTable)
}

getPerm5Num <- function(permObj) {
  minVal    <- min(permObj$avgTestAcc)
  maxVal    <- max(permObj$avgTestAcc)
  medianVal <- median(permObj$avgTestAcc)
  quantiles <- quantile(permObj$avgTestAcc)
  
  return (list(minimum = minVal,
               lowerQ  = quantiles[2],
               median  = medianVal,
               upperQ  = quantiles[4],
               maximum = maxVal))
}
