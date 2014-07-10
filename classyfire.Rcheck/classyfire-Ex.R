pkgname <- "classyfire"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('classyfire')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("avgTestAcc")
### * avgTestAcc

flush(stderr()); flush(stdout())

### Name: avgTestAcc
### Title: Average test accuracy for a classification ensemble
### Aliases: avgTestAcc

### ** Examples


data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

# Return the average test accuracy for the ensemble

avgTestAcc(obj)




cleanEx()
nameEx("avgTrainAcc")
### * avgTrainAcc

flush(stderr()); flush(stdout())

### Name: avgTrainAcc
### Title: Average train accuracy for a classification ensemble
### Aliases: avgTrainAcc

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")
# Return the average train accuracy for the ensemble

avgTrainAcc(obj)



cleanEx()
nameEx("confMatr")
### * confMatr

flush(stderr()); flush(stdout())

### Name: confMatr
### Title: Confusion matrix
### Aliases: confMatr

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

confMatr(1, obj)



cleanEx()
nameEx("ensRBF")
### * ensRBF

flush(stderr()); flush(stdout())

### Name: ensRBF
### Title: Optimisation of the RBF SVM tuning process via bootstrapping
### Aliases: ensRBF ensRBF.default
### Keywords: models multivariate models

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

length(obj)
attributes(obj[[1]])

print(round(obj[[1]]$testAcc, digits=2))
print(round(obj[[1]]$trainAcc, digits=2))



cleanEx()
nameEx("fiveNumSummary")
### * fiveNumSummary

flush(stderr()); flush(stdout())

### Name: fiveNumSummary
### Title: Five number summary
### Aliases: fiveNumSummary

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]

permObj <- runPermutation(iris, sampleIris, bootNum = 10, ensembleNum = 20, permNum = 5, runParallel = TRUE, cpus = 4, type = "SOCK")

fiveNumSummary(permObj)
fiveNumSummary(permObj)$median      
fiveNumSummary(permObj)$minimum
fiveNumSummary(permObj)$maximum
fiveNumSummary(permObj)$upperQ
fiveNumSummary(permObj)$lowerQ



cleanEx()
nameEx("ggClassBar")
### * ggClassBar

flush(stderr()); flush(stdout())

### Name: ggClassBar
### Title: Per class accuracies of correctly classified samples
### Aliases: ggClassBar

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")
              
ggClassBar(obj)
ggClassBar(obj , showText = TRUE)



cleanEx()
nameEx("ggClassPred")
### * ggClassPred

flush(stderr()); flush(stdout())

### Name: ggClassPred
### Title: Barplot of the per class accuracies.
### Aliases: ggClassPred

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

ggClassPred (obj, position = "dodge", showText = TRUE)
ggClassPred (obj, position = "stack")



cleanEx()
nameEx("ggEnsHist")
### * ggEnsHist

flush(stderr()); flush(stdout())

### Name: ggEnsHist
### Title: Ensemble Histograms
### Aliases: ggEnsHist

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

ggEnsHist(obj)
ggEnsHist(obj, density = TRUE)
ggEnsHist(obj, density = TRUE, percentiles=TRUE)
ggEnsHist(obj, density = TRUE, percentiles=TRUE, mean=TRUE)
ggEnsHist(obj, density = TRUE, percentiles=TRUE, median=TRUE)



cleanEx()
nameEx("ggPermHist")
### * ggPermHist

flush(stderr()); flush(stdout())

### Name: ggPermHist
### Title: Permutation Histograms
### Aliases: ggPermHist

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]

permObj <- runPermutation(iris, sampleIris, bootNum = 10, ensembleNum = 20, permNum = 5, runParallel = TRUE, cpus = 4, type = "SOCK")

ggPermHist(permObj)
ggPermHist(permObj, density=TRUE)
ggPermHist(permObj, density=TRUE, percentiles = TRUE, mean = TRUE)
ggPermHist(permObj, density=TRUE, percentiles = TRUE, median = TRUE)



cleanEx()
nameEx("ggbarTest")
### * ggbarTest

flush(stderr()); flush(stdout())

### Name: ggbarTest
### Title: Test Accuracies Scatter Plot
### Aliases: ggbarTest

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")
              
ggbarTest(obj)
ggbarTest(obj, showText = TRUE)
ggbarTest(obj, showText = TRUE, ylims=c(80, 100))



cleanEx()
nameEx("ggplotTrend")
### * ggplotTrend

flush(stderr()); flush(stdout())

### Name: ggplotTrend
### Title: Trend of the test accuracies
### Aliases: ggplotTrend

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")
              
ggplotTrend(obj)
ggplotTrend(obj, showText  = TRUE)
ggplotTrend(obj, showText  = TRUE, ylims=c(90, 100))



cleanEx()
nameEx("optimParams")
### * optimParams

flush(stderr()); flush(stdout())

### Name: optimParams
### Title: Optimal Paramaters
### Aliases: optimParams

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

optimParam <- optimParams(obj)
optimParam



cleanEx()
nameEx("overallClassPred")
### * overallClassPred

flush(stderr()); flush(stdout())

### Name: overallClassPred
### Title: Overal Class Predictions
### Aliases: overallClassPred

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

overallClassPred(obj)



cleanEx()
nameEx("runPermutation")
### * runPermutation

flush(stderr()); flush(stdout())

### Name: runPermutation
### Title: Permutation tests
### Aliases: runPermutation

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")

length(obj)
attributes(obj[[1]])

print(round(obj[[1]]$testAcc, digits=2))
print(round(obj[[1]]$trainAcc, digits=2))


permObj <- runPermutation(iris, sampleIris, bootNum = 10, ensembleNum = 20, permNum = 5, runParallel = TRUE, cpus = 4, type = "SOCK")
length(permObj)
attributes(permObj)

permObj$avgTestAcc
permObj$execTime
length(permObj$permList)



cleanEx()
nameEx("testAcc")
### * testAcc

flush(stderr()); flush(stdout())

### Name: testAcc
### Title: Test Accuracies (%CC)
### Aliases: testAcc

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")
              
# Return the test accuracies for every classifier in the ensemble

testAcc(obj)

# Return the train accuracies for every classifier in the ensemble

trainAcc(obj)



cleanEx()
nameEx("trainAcc")
### * trainAcc

flush(stderr()); flush(stdout())

### Name: trainAcc
### Title: Train Accuracies
### Aliases: trainAcc

### ** Examples

data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
obj <- ensRBF(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, 
              cpus = 4, type = "SOCK")
              
# Return the test accuracies for every classifier in the ensemble

testAcc(obj)

# Return the train accuracies for every classifier in the ensemble

trainAcc(obj)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
