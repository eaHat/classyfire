# Plot functions

# plot the test accuracies of the ensemble of classifiers

ggbarTest <- function(ensObj, xlabel = NULL, ylabel=NULL, showText=FALSE, xlims=NULL, ylims= NULL, ...) {
  
  acc = getTestAcc(ensObj)
  ens = length(ensObj)
  
  testDat = cbind (1:ens, acc)
  colnames(testDat) = c("Ensemble","Accuracy")
  testDat = as.data.frame(testDat)
  
  p <- ggplot(testDat, aes(Ensemble, Accuracy)) + geom_point(aes(colour = Accuracy), size = 2.3) + theme(legend.position = "none") 
  
  if (is.null(xlabel)) {xlabel ="Ensemble Iteration"}
  if (is.null(ylabel)) {ylabel ="Test Accuracy"}
  
  p <- p +xlab(xlabel) + ylab(ylabel) 
  
  if (showText == TRUE) {p <- p+geom_text(data = testDat, aes(Ensemble, Accuracy, label = paste(round(Accuracy),"%",sep="")), vjust = -0.8, size=3.5) }
  
  if (length(xlims) == 2) { p <- p + xlim(xlims)}
  if (length(ylims) == 2) { p <- p + ylim(ylims)}
  
  return (p)
}


ggplotTrend <- function(ensObj, xlabel = NULL, ylabel=NULL, showText=FALSE, xlims=NULL, ylims= NULL, ...) {
  
  acc = getTestAcc(ensObj)
  ens = length(ensObj)
  meanVal = acc[1]
  
  for (i in 2:length(acc)) {
    meanVal = c(meanVal, mean(c(meanVal, acc[i])))
  }
  
  dataMatr = cbind (1:ens, meanVal)
  colnames(dataMatr) = c("Ensemble","Avg_Accuracy")
  dataMatr = as.data.frame(dataMatr)
  
  p <- ggplot(dataMatr, aes(Ensemble, Avg_Accuracy)) + geom_point(aes(colour = Avg_Accuracy), size = 2.3) + geom_line(linetype="dotted") + theme(legend.position = "none")  
  
  if (is.null(xlabel)) {xlabel ="Ensemble Iteration"}
  if (is.null(ylabel)) {ylabel ="Average Test Accuracy"}
  
  p <- p +xlab(xlabel) + ylab(ylabel) 
  
  if (showText == TRUE) {p <- p + geom_text(data = dataMatr, aes(Ensemble, Avg_Accuracy, label = paste(round(Avg_Accuracy),"%",sep="")), vjust = -0.8, size=3.5)  }
  
  if (length(xlims) == 2) { p <- p + xlim(xlims)}
  if (length(ylims) == 2) { p <- p + ylim(ylims)}
  
  return(p)
}



# create.report <- function(ensObj)
# {
#   rnw.file<-'SweaveTest2.Rnw'
#   brew('SweaveTest.Rnw', rnw.file)
#   knit(rnw.file)
#   
#   texi2pdf("SweaveTest2.tex", clean = TRUE, quiet = TRUE)
#   out.file <- "SweaveTest2.pdf")
#   return(out.file)
# 


ggEnsHist <- function (ensObj, density = FALSE, percentiles = FALSE, mean = FALSE, median = FALSE) {
  accuracies = getTestAcc(ensObj)
  accuracies = as.data.frame(accuracies)
  colnames(accuracies) = "Accuracies"
  avgVal = avgTestAcc(ensObj)
  
  upper = mean(as.vector(as.matrix(accuracies))) + 2*sd(as.vector(as.matrix(accuracies)))
  lower = mean(as.vector(as.matrix(accuracies))) - 2*sd(as.vector(as.matrix(accuracies)))
  
  
  if (density == TRUE){
    m <- ggplot(accuracies, aes(x=Accuracies, y=..density..)) + 
      geom_histogram(binwidth=2, colour="#999999", fill="white") +
      geom_density(alpha=.2, fill="white", colour="#333333")  
  } else {
    m <- ggplot(accuracies, aes(x=Accuracies, y=..count..)) + 
      geom_histogram(binwidth=2, colour="#999999", fill="white")
  }
  
  if (percentiles == TRUE){
    m <- m + geom_vline(xintercept=lower, color="purple", linetype="dashed", size=0.8) + 
      geom_vline(xintercept=upper, color="purple", linetype="dashed", size=0.8)  
  }
  
  if (mean == TRUE){
    m <- m + geom_vline(xintercept=mean(getTestAcc(ensObj)), color="red", linetype="dashed", size=0.8) 
  }
  
  if (median == TRUE){
    m <- m + geom_vline(xintercept=median(getTestAcc(ensObj)), color="cyan", linetype="dashed", size=0.8) 
  }
  
  return(m)
}

# Correctly Classified Samples

ggClassBar <- function(ensObj, showText = FALSE, ...) {
  classPred = cbind(colnames(overallClassPred(ensObj)), diag(overallClassPred(ensObj)))
  classPred = as.data.frame(classPred)
  colnames(classPred) = c("Class", "Percentage")
    
  p<-ggplot(data=classPred, aes(x=Class, y=as.numeric(as.vector(Percentage)), fill=Class)) + 
    geom_bar(width=0.5, stat="identity", ...)+ theme_bw() + 
    xlab("\nClasses") + ylab("Percentages of Correctly Classified Samples per Class (%)\n") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
  
  if (showText == TRUE) {
    p <- p + geom_text(data=classPred, aes(x=Class, y=as.numeric(as.vector(Percentage)), label=paste(as.vector(Percentage),"%",sep="")), vjust=-0.5, size=3.7) 
  }
  
  return(p)
}


# Plot All Class Pred 

ggClassPred <- function(ensObj, position = "stack", showText=FALSE, xlabel = NULL, ylabel=NULL, ...) {
  classPred = as.data.frame(ftable(overallClassPred(ensObj), row.vars = 2:1))
  colnames(classPred) = c("PredClass", "OriginalClass", "Percentage")
  
  p<-ggplot(classPred, aes(OriginalClass, y=as.numeric(as.vector(Percentage)), fill=PredClass, ymin=0, ymax=100)) + 
      geom_bar(width=0.5, stat="identity", position = position, ...)+ theme_bw() + 
     theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
  
  if (is.null(xlabel)) {xlabel ="\nClasses"}
  if (is.null(ylabel)) {ylabel ="Percentages of Class Predictions (%)\n"}
  
  p <- p +xlab(xlabel) + ylab(ylabel)
  
  if (showText == TRUE) {
    if (position == "stack") {
      p <- p + geom_text(data=classPred, aes(x=OriginalClass, y=as.numeric(as.vector(Percentage)), label=ifelse(Percentage>0, paste(as.vector(Percentage),"%",sep=""), "")), size=3.7, position = "stack", vjust = 1.2) 
    } else {
#       p <- p + geom_text(data=classPred, aes(x=OriginalClass, y=as.numeric(as.vector(Percentage)), label=paste(as.vector(Percentage),"%",sep="")), size=3.7, position = position_dodge(width=0.5/length(levels(ensObj[[1]]$testClass)))) 
      p <- p + geom_text(data=classPred, aes(x=OriginalClass, y=as.numeric(as.vector(Percentage)), label=paste(as.vector(Percentage),"%",sep="")), size=3.7, position = position_dodge(width=0.5))
    }
  }
  
  return(p)
}


ggPermHist <- function(permObj, density = FALSE, percentiles = FALSE, mean = FALSE, median = FALSE, ...) {
  
  accuracies = permObj$avgTestAcc
  accuracies = as.data.frame(accuracies)
  colnames(accuracies) = "Accuracies"
  
  upper = mean(as.vector(as.matrix(accuracies))) + 2*sd(as.vector(as.matrix(accuracies)))
  lower = mean(as.vector(as.matrix(accuracies))) - 2*sd(as.vector(as.matrix(accuracies)))
  
  if (density == TRUE){
    m <- ggplot(accuracies, aes(x=Accuracies, y=..density..))+ 
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
    m <- ggplot(accuracies, aes(x=Accuracies, y=..count..)) + 
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

# permAnalysis <- function(ensObj, permObj, type=c("hist", "val")) {
#   accuracies1 = testAcc(ensObj)
#   accuracies1 = as.data.frame(accuracies1)
#   colnames(accuracies1) = "Accuracies"
#   
#   upper1 = mean(as.vector(as.matrix(accuracies1))) + 2*sd(as.vector(as.matrix(accuracies1)))
#   lower1 = mean(as.vector(as.matrix(accuracies1))) - 2*sd(as.vector(as.matrix(accuracies1)))
#   avgVal = avgTestAcc(ensObj)
#   
#   m1 <- ggplot(accuracies1, aes(x=Accuracies, y=..density..)) + 
#     geom_histogram(binwidth=2, colour="#999999", fill="white") +
#     geom_density(alpha=.2, fill="white", colour="#333333") + 
#     geom_vline(xintercept=avgVal, color="blue", size=1)  + 
#     geom_vline(xintercept=lower1, color="purple", linetype="dashed", size=0.8) + 
#     geom_vline(xintercept=upper1, color="purple", linetype="dashed", size=0.8) 
#   
#   
#   accuracies2 = permObj$avgTestAcc
#   accuracies2 = as.data.frame(accuracies2)
#   colnames(accuracies2) = "Accuracies"
#   
#   upper2 = mean(as.vector(as.matrix(accuracies2))) + 2*sd(as.vector(as.matrix(accuracies2)))
#   lower2 = mean(as.vector(as.matrix(accuracies2))) - 2*sd(as.vector(as.matrix(accuracies2)))
#   
#   m <- ggplot(accuracies2, aes(x=Accuracies, y=..density..)) + 
#     geom_histogram(binwidth=2, colour="#999999", fill="white") +
#     geom_density(alpha=.2, fill="white", colour="#333333")   
#   
#   if (type == "val") {
#     m <- m + geom_vline(xintercept=avgVal, color="blue", size=1)   
#   } 
#   
#   if (type == "hist") {
#     m <- m + m1 
#   }
#   
#   return (m)
# }


# plotPermHist <- function(permObj, ensObj= NULL, showNonPerm = FALSE, colNonPerm = "red", ...) {
#   if (){
#     hist(permObj$avgTestAcc, xlab="Test Accuracies", main= "Histogram of Permutation Accuracies")
#     abline(v=avgTestAcc(ensObjj), col= colNonPerm)
#   } else {
#     hist(permObj$avgTestAcc, xlab="Test Accuracies", xlim=c((min(permObj$avgTestAcc)-5), (max(permObj$avgTestAcc)+5)), main= "Histogram of Permutation Accuracies")
#   }
# }
# 
# 
# plotPermDens <- function(permObj, ensObj= NULL, showNonPerm = FALSE, colNonPerm = "red", ...) {
#   if (){
#     hist(permObj$avgTestAcc, xlab="Test Accuracies", xlim=c(0,100), main= "Histogram of Permutation Accuracies")
#     abline(v=avgTestAcc(ensObj), col= colNonPerm)
#   } else {
#     hist(permObj$avgTestAcc, xlab="Test Accuracies", xlim=c((min(permObj$avgTestAcc)-5), (max(permObj$avgTestAcc)+5)), main= "Histogram of Permutation Accuracies")
#   }
# }