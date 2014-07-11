\name{ensRBF}
\alias{ensRBF}
\alias{ensRBF.default}
\title{
Optimisation of the RBF SVM tuning process via bootstrapping
}
\description{
The ensRBF function introduces a new heuristic methodology for speeding up the optimisation process 
of the RBF SVM hyperparameters via bootstrapping. In this novel approach, a fast and robust approximation algorithm for constrained nonlinear optimisation, the Box complex algorithm, is used 
to replace the widely applied yet computationally intensive grid-search. The Box complex algorithm 
in addition to parallel programming is used as a means of significantly minimising 
the computational complexity and overall execution time as well as improving the overall performance of the classifiers.
}
\usage{
ensRBF(inputData, inputClass, ...)

\method{ensRBF}{default}(inputData, inputClass, bootNum = 100, ensembleNum = 100, 
  			runParallel=TRUE, cpus = NULL, type = "SOCK", socketHosts = NULL)
}
\arguments{
  \item{inputData}{The input data matrix as provided by the user (mandatory field).}
  \item{inputClass}{The input class vector as provided by the user (mandatory field).}
  \item{bootNum}{The number of bootstrap iterations during the optimisation process. By default, the value is set to 100.}
  \item{ensembleNum}{The number of classifiers that form the generated ensemble of classifiers. By default, the value is set to 100.}
  \item{runParallel}{Boolean value that determines parallel or sequential execution.}
  \item{cpus}{Numeric value that provides the number of CPUs requested for the cluster.}
  \item{type}{The type of cluster. It can take the values 'SOCK', 'MPI', 'PVM' or 'NWS'. By default, type is equal to 'SOCK'}
  \item{socketHosts}{Host list for socket clusters. Only needed for socketmode (SOCK) and if using more than one machines (if using only your local machine (localhost) no list is needed).}
  \item{...}{The remaining optional fields.}
}
\value{
The ensRBF function returns an object in the form of an R list. The attributes of the list can be accessed by executing the "attributes" command. More specifically, the list of attributes includes:
	\item{testAcc}{The test accuracy (\%CC) (numerical value)  of a single classifier in the ensemble.}
	\item{trainAcc}{The train accuracy (numerical value) of a single classifier in the ensemble.}
	\item{conf_matrix}{The confusion matrix of a single classifier.}
	\item{prop_table}{Similar to the confusion matrix, the per class (\%) accuracies.}
	\item{predClass}{The vector of classes for the test class as predicted by the SVM model.}
	\item{testClass}{The vector of true classes of the test class.}
	\item{missNames}{In case that the names of the samples (rows) are supplied in the input data matrix, the missNames attribute returns the names of the missclassified samples.}
	\item{accNames}{In case that the names of the samples (rows) are supplied in the input data matrix, the accNames attribute returns the names of the correctly classified samples.}
	\item{trainClass}{The vector of classes used in the training of the SVM.}
	\item{testData}{The data matrix (individual test set) used for testing.}
	\item{trainData}{The data matrix (individual test set) used for training.}
	\item{testSamples}{The randomly selected samples (rows) that were used in this instance to create the }
	\item{bootObj}{The matrix of bootstrapped samples based on the provided bootNum.}
	\item{optGamma}{Optimal gamma value }
	\item{optCost}{Optimal cost value for the RBF SVM }
	\item{runTime}{The execution time for a single classifier within the ensemble.}
	\item{svmModel}{The created SVM model as an R object.}
}
\details{For a given input dataset D, a random fraction of samples is removed and kept aside as an independent test set during the training process of the model. This selection of samples forms the dataset Dtest. This test set typically comprises a third of the original samples. Using a stratified holdout approach, the test set consists of the same balance of sample classes as the initial dataset D. The remaining samples that are not selected, form the training set Dtrain. Since the test set is kept aside during the whole training process, the risk of overfitting is minimised.

In the case of bootstrapping, a bootstrap training set DbootTrain is created by randomly picking n samples with replacement from the training dataset Dtrain. The total size of DbootTrain is equal to the size of Dtrain. Since bootstrapping is based on sampling with replacement, any given sample could be present multiple times within the same bootstrap training set. The remaining samples not found in the bootstrap training set make up the bootstrap test set DbootTest. To avoid reliance on one specific bootstrapping split, bootstrapping is repeated at least 100 times until a clear winning parameter combination emerges. 

Ultimately, the optimal parameters are used to train a new classifier with the full Dtrain dataset and test it on the independent test set Dtest, which has been left aside during the entire optimisation process. Even though the approach described thus far generates an excellent classifier, the random selection of test samples in the initial split may have been fortunate. For a more accurate and reliable overview, the whole process is repeated a minimum of 100 times until a stable average classification rate emerges. The output of this repetition consists of at least 100 individual classification models built using the optimum parameter settings. Rather than isolating a single classifier, all individual classification models are fused into a classification ensemble. 
}
\note{The ensRBF function does not force an upper limit for the bootNum, ensembleNum and cpus parameters to the users. However, it is advisable not to use extremely high values for these parameters.}
\references{
	\url{http://www.classyfire.org}
}
\seealso{
	\code{\link{runPermutation}}
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
}
\keyword{models}
\keyword{multivariate}
\keyword{models}