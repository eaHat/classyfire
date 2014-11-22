## Tests

data(iris)

irisClass <- iris[,5]
irisData  <- iris[,-5]

# Construct a classification ensemble with 100 classifiers and 100 bootstrap 
# iterations during optimisation

ens <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 10, 
               ensNum = 50, parallel = TRUE, cpus = 4, type = "SOCK")

# List of attributes available for each classifier in the ensemble
attributes(ens)

# The test and train accuracy of the first classifier in the ensemble 
ens$testAcc[1]
ens$trainAcc[1]

# Get the overall average Test and Train accuracy
getAvgAcc(ens)$Test
getAvgAcc(ens)$Train

# All the test accuracies and the average test accuracy in the ensemble
ens$testAcc    # alternatively, getAcc(ens)$Test
ens$trainAcc   # alternatively, getAcc(ens)$rain



# Randomly generate test data to find out their classes using the generated ensemble
# 400 points are selected at random, which results in 100 samples (rows).

testMatr <- matrix(runif(400)*100, ncol = ncol(irisData))           

# Predict the classes of the data using the classifiers in the constructed ensemble

predRes <- cfPredict(ens, testMatr)


