cfPredict <- function(ensObj, newInputData) {
  if (missing(newInputData) || is.null(newInputData)) {
    stop("Please provide a matrix with test data.")
  } else {
    newInputData = as.matrix(newInputData)
    
    predMatr = finalClasses = confScores = c()
    
    for (i in 1:length(ensObj)) {
      newModel <- ensObj[[i]]$svmModel
      predTest <- predict(newModel, newInputData)
      predMatr <- cbind(predMatr, as.character(predTest))
    }
    
    colnames(predMatr) <- paste("model", 1:length(ensObj), sep="")
    if (is.null(rownames(predMatr))) {rownames(predMatr) = paste("sample", 1:nrow(predMatr), sep="")}
    
    confList <- lapply(apply (predMatr, 1, function(x) list(table(x))), "[[", 1)
    
    for (j in 1:length(confList)) {
      finalClasses <- c(finalClasses, names(which.max(confList[[j]])))
      confScores   <- c(confScores, max(prop.table(confList[[j]]) * 100))
    }
    
    finalMatr <- as.data.frame(cbind(finalClasses, as.numeric(confScores)))
    rownames(finalMatr) <- rownames(newInputData)
    colnames(finalMatr) <- c("Voted Class", "Conf Score(%)")
    
    return (finalMatr)
  }
}
