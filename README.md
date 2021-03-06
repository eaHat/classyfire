classyfire
==========

classyfire is an R (www.r-project.org) package for robust multivariate classification using highly optimised SVM ensembles. The latest stable version is available on CRAN at 
[http://cran.r-project.org/web/packages/classyfire/index.html](http://cran.r-project.org/web/packages/classyfire/index.html)

The classyfire package provides a collection of functions for the creation and application of highly optimised, robustly evaluated ensembles of support vector machines (SVMs). The package takes care of training individual SVM classifiers using a fast parallel heuristic algorithm, and combines individual classifiers into ensembles. Robust metrics of classification performance are offered by bootstrap resampling and permutation testing.




### Usage

Install from CRAN and load in the R console
```R
install.packages("classyfire")
library(classyfire)
??classyfire
```

### Building a classification ensemble

Loading some test data, for instance the *iris* dataset
```R
data(iris)

irisClass <- iris[,5]
irisData  <- iris[,-5]
```

Construct a classification ensemble **in parallel** (using 4 cpus in this instance) that consists of 10 independent classification models (classifiers) optimised using 10 bootstrap iterations
```R
ens <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 10, ensNum = 10, 
               parallel = TRUE, cpus = 4, type = "SOCK")
```

Similarly, **in sequence**: 
```R
ens <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 10, ensNum = 10, 
               parallel = FALSE)
```

### Testing new unknown data (in this instance, random data)

```R
testMatr <- matrix(runif(400)*100, ncol = ncol(irisData))           
predRes  <- cfPredict(ens, testMatr)
```

### Determining statistical significance by permutation testing

```R
permObj <- cfPermute(irisData, irisClass, bootNum = 10, ensNum = 10, permNum = 5,
                     parallel = TRUE, cpus = 4, type = "SOCK")
```

### Evaluating the classification ensemble

All the functions for descriptive statistics within classyfire start with the prefix "**get**". For example: 

```R
getAvgAcc(ens)
getAcc(ens)
getConfMatr(ens)
getOptParam(ens)
getPerm5Num(permObj)
```

### Plotting functions within classyfire

All the functions for plotting within classyfire start with the prefix "**gg**" since the R package *ggplot2* is in use. For example: 

```R
ggClassPred(ens)
ggEnsTrend(ens)
ggEnsHist(ens)
ggPermHist(permObj)
ggFusedHist(ensObj, permObj)
```
