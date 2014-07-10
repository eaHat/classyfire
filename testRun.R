setwd("/Library/WebServer/CGI-Executables/newPack")

rm(list = ls(all = TRUE))
graphics.off()


library(e1071)
library(boot)
library(neldermead)



data(iris)

sampleIris = iris[,5]
iris = iris[,-5]
            
system.time(obj <- ensRBF.default(iris, sampleIris, bootNum = 10, ensembleNum = 20, runParallel = TRUE, cpus = 4, type = "SOCK"))
            

bootNum = 20
ensembleNum = 10

irisObj = ensRBF(iris, sampleIris, runParallel=TRUE, slavesNum=4, bootNum=10, ensembleNum = 5)
print(mean(sapply(irisObj$parComplexRes,"[[",1)))
for (i in 1:10){print (irisObj$parComplexRes[[i]]$accTest)}

classAcc = c(classAcc, mean(sapply(parComplexRes,"[[",1)))


data(mtcars)
sampleMtcars = mtcars[,-11]
mtcars = mtcars[,1:7]
pcMt = pca(mtcars, plotPCA = TRUE, sensory=sampleMtcars)
