\name{runPermutation}
\alias{runPermutation}
\title{
Permutation tests
}
\description{
The function runPermutation performs permutation tests for an ensemble of RBF SVM models validated with bootstrapping
}
\usage{
runPermutation(inputData, inputClass, bootNum = 100, ensembleNum = 100, permNum=100, 
      runParallel=TRUE, cpus=NULL, type = "SOCK", socketHosts = NULL, progressBar = TRUE)
}
\arguments{
  \item{inputData}{The input data matrix as provided by the user (mandatory field).}
  \item{inputClass}{The input class vector as provided by the user (mandatory field).}
  \item{bootNum}{The number of bootstrap iterations during the optimisation process. By default, the value is set to 100.}
  \item{ensembleNum}{The number of classifiers that constitute the ensemble for each permutation. By default, the value is set to 100.}
  \item{permNum}{The number of permutations to be executed. By default, the value is set to 100.}
  \item{runParallel}{Boolean value that determines parallel or sequential execution.}
  \item{cpus}{Numeric value that provides the number of CPUs requested for the cluster.}
  \item{type}{The type of cluster. It can take the values 'SOCK', 'MPI', 'PVM' or 'NWS'. By default, type is equal to 'SOCK'}
  \item{socketHosts}{Host list for socket clusters. Only needed for socketmode (SOCK) and if using more than one machines (if using only your local machine (localhost) no list is needed).}
  \item{progressBar}{Boolean value that determines whether a progress bar should be displayed.}
}
\details{
Permutation testing is a widely-applied process used in order to provide an indication of the statistical significance of the classification results. In a permutation test, the entries of the original class vector (inputClass) are randomly shuffled, while the class distribution is preserved. This approach destroys all the sample membership information since the samples of a permuted dataset correspond to randomly assigned classes. The whole model building process as described \code{\link{ensRBF}} is once more repeated for the "false" (permuted) classes. In general, permutation testing is performed at least 100 times until a stable distribution of results is obtained.  
}
\author{
Eleni Chatzimichali <hatzimihali.eleni@gmail.com>,
Conrad Bessant <c.bessant@qmul.ac.uk>
}
\examples{
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
}
