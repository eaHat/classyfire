# Plot functions

ggEnsTrend <- function(ensObj, xlabel = NULL, ylabel=NULL, showText=FALSE, xlims=NULL, ylims= NULL, ...) {
  Ensemble <- Avg_Acc <- NULL 
  
  ensAcc  <- getAcc(ensObj)$Test
  meanVal <- ensAcc[1]
  
  for (i in 2:length(ensAcc)) {
    meanVal <- c(meanVal, mean(ensAcc[1:i]))
  }
  
  avgAcc <- cbind (1:length(ensObj), meanVal)
  avgAcc <- as.data.frame(avgAcc)
  colnames(avgAcc) <- c("Ensemble","Avg_Acc")
  
  p <- ggplot(avgAcc, aes(Ensemble, Avg_Acc)) + geom_point(aes(colour = Avg_Acc), size = 2.3) + geom_line(linetype="dotted") + theme(legend.position = "none")  
  
  if (is.null(xlabel)) {xlabel ="Ensemble Iteration"}
  if (is.null(ylabel)) {ylabel ="Average Test Accuracy"}
  
  p <- p +xlab(xlabel) + ylab(ylabel) 
  
  if (showText == TRUE) {p <- p + geom_text(data = avgAcc, aes(Ensemble, Avg_Acc, label = paste(round(Avg_Acc, digits=2),"%",sep="")), vjust = -0.8, size=3.5)  }
  
  if (length(xlims) == 2) { p <- p + xlim(xlims)}
  if (length(ylims) == 2) { p <- p + ylim(ylims)}
  
  return(p)
}


ggEnsHist <- function (ensObj, density = FALSE, percentiles = FALSE, mean = FALSE, median = FALSE) {
  Accuracies <- ..density.. <- ..count.. <- NULL 
  
  avgVal <- getAvgAcc(ensObj)$Test
  ensAcc <- getAcc(ensObj)$Test
  ensAcc <- as.data.frame(ensAcc)
  colnames(ensAcc) <- "Accuracies"
  
  upper <- mean(as.vector(as.matrix(ensAcc))) + 2*sd(as.vector(as.matrix(ensAcc)))
  lower <- mean(as.vector(as.matrix(ensAcc))) - 2*sd(as.vector(as.matrix(ensAcc)))
  
  if (density == TRUE){
    m <- ggplot(ensAcc, aes(x=Accuracies, y=..density..)) + 
      geom_histogram(binwidth=2, colour="#999999", fill="white") +
      geom_density(alpha=.2, fill="white", colour="#333333")  
  } else {
    m <- ggplot(ensAcc, aes(x=Accuracies, y=..count..)) + 
      geom_histogram(binwidth=2, colour="#999999", fill="white")
  }
  
  if (percentiles == TRUE){
    m <- m + geom_vline(xintercept=lower, color="purple", linetype="dashed", size=0.8) + 
      geom_vline(xintercept=upper, color="purple", linetype="dashed", size=0.8)  
  }
  
  if (mean   == TRUE){ m <- m + geom_vline(xintercept=mean(getAcc(ensObj)$Test), color="red", linetype="dashed", size=0.8)  }
  
  if (median == TRUE){ m <- m + geom_vline(xintercept=median(getAcc(ensObj)$Test), color="cyan", linetype="dashed", size=0.8) }
  
  return(m)
}

# Correctly Classified Samples

ggClassPred <- function(ensObj, position = "stack", displayAll = FALSE, showText=FALSE, xlabel = NULL, ylabel=NULL, ...) {
  InitClass <- PredClass <- Class <- Percentage <- predictions <- p <- NULL 
  
  if (displayAll == FALSE) {
    predictions <- as.data.frame(cbind(colnames(getConfMatr(ensObj)), diag(getConfMatr(ensObj))))
    colnames(predictions) = c("Class", "Percentage")
    
    p<-ggplot(data = predictions, aes(x = Class, y=as.numeric(as.vector(Percentage)), fill=Class)) + 
      geom_bar(width=0.5, stat="identity", ...)+ theme_bw() + 
      xlab("\nClasses") + ylab("Percentages of Correctly Classified Samples per Class (%)\n") +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
    
    if (showText == TRUE) {
      p <- p + geom_text(data=predictions, aes(x=Class, y=as.numeric(as.vector(Percentage)), 
                                               label=paste(as.vector(Percentage),"%",sep="")), 
                                               vjust=-0.5, size=3.7) 
    }
  } else {
    predictions <- as.data.frame(ftable(getConfMatr(ensObj), row.vars = 2:1))
    colnames(predictions) <- c("PredClass", "InitClass", "Percentage")
    
    p<-ggplot(predictions, aes(InitClass, y=as.numeric(as.vector(Percentage)), fill=PredClass, ymin=0, ymax=100)) + 
      geom_bar(width=0.5, stat="identity", position = position, ...)+ theme_bw() + 
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
    
    if (showText == TRUE) {
      if (position == "stack") {
        p <- p + geom_text(data=predictions, aes(x=InitClass, y=as.numeric(as.vector(Percentage)), 
                                                 label=ifelse(Percentage>0, paste(as.vector(Percentage),"%",sep=""), "")), 
                           size=3.7, position = "stack", ...) 
      } else {
        p <- p + geom_text(data=predictions, aes(x=InitClass, y=as.numeric(as.vector(Percentage)), 
                                                 label=paste(as.vector(Percentage),"%",sep="")), size=3.7, 
                           position = position_dodge(width=0.5))
      }
    }
  }
  
  if (is.null(xlabel)) {xlabel ="\nClasses"}
  if (is.null(ylabel)) {ylabel ="Percentages of Class Predictions (%)\n"}
  
  p <- p +xlab(xlabel) + ylab(ylabel)
  
  return(p)
}


ggPermHist <- function(permObj, density = FALSE, percentiles = FALSE, mean = FALSE, median = FALSE, ...) {
  Accuracies <- x <- y <- ..density.. <- ..count.. <- NULL 
  
  permAcc <- as.data.frame(permObj$avgTestAcc)
  colnames(permAcc) <- "Accuracies"
  
  upper <- mean(as.vector(as.matrix(permAcc))) + 2*sd(as.vector(as.matrix(permAcc)))
  lower <- mean(as.vector(as.matrix(permAcc))) - 2*sd(as.vector(as.matrix(permAcc)))
  
  if (density == TRUE){
    m <- ggplot(permAcc, aes(x=Accuracies, y=..density..))+ 
          geom_histogram(binwidth=2, colour="#999999", fill="white") + 
          geom_density(alpha=.2, fill="white", colour="#333333")  
    
    p <- print(m)
    
    temp = which((p$data[[2]]$x > upper) == TRUE, arr.ind=TRUE)
    dd1 = p$data[[2]][temp,]
    temp = which((p$data[[2]]$x < upper) == TRUE, arr.ind=TRUE)
    dd2 = p$data[[2]][temp,]
    
    m <- m + layer(data = dd1, mapping = aes(x=x, y=y), geom = "area", geom_params=list(fill="red" ,alpha=.3))+
      layer(data = dd2, mapping = aes(x=x, y=y), geom = "area", geom_params=list(fill="white" ,alpha=.3)) 
    
  } else {
    m <- ggplot(permAcc, aes(x=Accuracies, y=..count..)) + 
      geom_histogram(binwidth=2, colour="#999999", fill="white")
  }
  
  if (percentiles == TRUE){
    m <- m + geom_vline(xintercept=lower, color="purple", linetype="dashed", size=0.8) + 
      geom_vline(xintercept=upper, color="purple", linetype="dashed", size=0.8)  
  }
  
  if (mean == TRUE){
    m <- m + geom_vline(xintercept=mean(permObj$avgTestAcc), color="red", linetype="dashed", size=0.8) 
  }
  
  if (median == TRUE){
    m <- m + geom_vline(xintercept=median(permObj$avgTestAcc), color="cyan", linetype="dashed", size=0.8) 
  }
  
  return (m)
}
