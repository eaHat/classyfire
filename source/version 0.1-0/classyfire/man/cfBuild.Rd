\name{cfBuild}
\alias{cfBuild}
\alias{cfBuild.default}
\title{
Create a highly optimised ensemble of RBF SVM classifiers
}
\description{
The \code{cfBuild} function creates a highly optimised ensemble of radial basis function (RBF) support vector machines (SVMs). The \code{cfBuild} function takes care of all aspects of the SVM optimisation, internally splitting the supplied data into separate training and testing subsets using a bootstrapping approach coupled with a heuristic optimisation algorithm and parallel processing to minimise computation time. The ensemble object can then be used to classify newly acquired data using the \code{\link{cfPredict}} function. 
}
\usage{
cfBuild(inputData, inputClass, ...)

\method{cfBuild}{default}(inputData, inputClass, bootNum = 100, ensNum = 100, 
      	parallel=TRUE, cpus = NULL, type = "SOCK", socketHosts = NULL, ...)
}
\arguments{
  \item{inputData}{The input data matrix as provided by the user (mandatory field).}
  \item{inputClass}{The input class vector as provided by the user (mandatory field).}
  \item{bootNum}{The number of bootstrap iterations in the optimisation process. By default, the bootNum value is set to 100.}
  \item{ensNum}{The number of classifiers that form the classification ensemble. By default, the ensNum value is set to 100.}
  \item{parallel}{Boolean value that determines parallel or sequential execution. By default set to \code{TRUE}. For more details, see \link{sfInit}.}
  \item{cpus}{Numeric value that provides the number of CPUs requested for the cluster. For more details, see \link{sfInit}.}
  \item{type}{The type of cluster. It can take the values 'SOCK', 'MPI', 'PVM' or 'NWS'. By default, type is equal to 'SOCK'. For more details, see \link{sfInit}.}
  \item{socketHosts}{Host list for socket clusters. Only needed for socketmode (SOCK) and if using more than one machines (if using only your local machine (localhost) no list is needed). For more details, see \link{sfInit}.}
  \item{...}{The remaining optional fields.}
}
\value{
The \code{cfBuild} function returns an object in the form of an R list. The attributes of the list can be accessed by executing the \link{attributes} command. More specifically, the list of attributes includes:
  \item{testAcc}{The test accuracy (\%CC) of a single classifier in the ensemble.}
	\item{trainAcc}{The train accuracy of a single classifier in the ensemble.}
	\item{optGamma}{Optimal gamma value for a single SVM model}
  \item{optCost}{Optimal cost value for a single SVM model}
  \item{runTime}{The execution time for a single classifier within the ensemble.}
  \item{confMatr}{The confusion matrix of a single classifier.}
	\item{propTable}{Similar to the confusion matrix, the per class (\%) accuracies.}
	\item{predClass}{The vector of classes for the test class as predicted by the SVM model.}
	\item{testClass}{The vector of true classes of the test class.}
	\item{missNames}{In case that the names of the samples (rows) are supplied in the input data matrix, the \code{missNames} attribute returns the names of the missclassified samples.}
	\item{accNames}{In case that the names of the samples (rows) are supplied in the input data matrix, the \code{accNames} attribute returns the names of the correctly classified samples.}
	\item{trainClass}{The vector of classes used in the training of the SVM.}
	\item{testData}{The data matrix (individual test set) used for testing.}
	\item{trainData}{The data matrix (individual test set) used for training.}
	\item{testSamples}{The randomly selected samples (rows) that were used in this instance to create the train and test data.}
	\item{bootObj}{The matrix of bootstrapped samples based on the provided \code{bootNum}.}
	\item{svmModel}{The created SVM model as an R object.}
}
\details{For a given input dataset \eqn{D}, a random fraction of samples is removed and kept aside as an independent test set during the training process of the model. This selection of samples forms the dataset \eqn{D_{test}}{Dtest}. This test set typically comprises a third of the original samples. Using a stratified holdout approach, the test set consists of the same balance of sample classes as the initial dataset \eqn{D}. The remaining samples that are not selected, form the training set \eqn{D_{train}}{Dtrain}. Since the test set is kept aside during the whole training process, the risk of overfitting is minimised.\cr\cr
In the case of bootstrapping, a bootstrap training set \eqn{D_{bootTrain}}{DbootTrain} is created by randomly picking \eqn{n} samples with replacement from the training dataset \eqn{D_{train}}{Dtrain}. The total size of \eqn{D_{bootTrain}}{DbootTrain} is equal to the size of \eqn{D_{train}}{Dtrain}. Since bootstrapping is based on sampling with replacement, any given sample could be present multiple times within the same bootstrap training set. The remaining samples not found in the bootstrap training set make up the bootstrap test set \eqn{D_{bootTest}}{DbootTest}. To avoid reliance on one specific bootstrapping split, bootstrapping is repeated at least \eqn{bootNum} times until a clear winning parameter combination emerges. \cr\cr
Ultimately, the optimal parameters are used to train a new classifier with the full \eqn{D_{train}}{Dtrain} dataset and test it on the independent test set \eqn{D_{test}}{Dtest}, which has been left aside during the entire optimisation process. Even though the approach described thus far generates an excellent classifier, the random selection of test samples in the initial split may have been fortunate. For a more accurate and reliable overview, the whole process should be repeated a minimum of 100 times (default value of \eqn{ensNum}) or until a stable average classification rate emerges. The output of this repetition consists of at least \eqn{ensNum} individual classification models built using the optimum parameter settings. Rather than isolating a single classifier, all individual classification models are fused into a classification ensemble. 
}
\note{
  \itemize{
    \item
    Data are scaled internally, usually yielding better results. The parameters of SVM-models usually \emph{must} be tuned to yield sensible results! For more information, see function \link{svm}.

    \item
    The \code{cfBuild} function does not force an upper limit for the bootNum, ensNum and cpus parameters to the users. However, it is advisable not to use extremely high values for these parameters.
  }
}
\seealso{
  \code{\link{cfPredict}},
	\code{\link{cfPermute}}
}
\author{
Adapted functionality by Eleni Chatzimichali (ea.chatzimichali@gmail.com)\cr
Author of the SVM functions: David Meyer (<David.Meyer@R-project.org>)\cr
(based on C/C++-code by Chih-Chung Chang and Chih-Jen Lin)\cr
Author of Scilab neldermead module: Michael Baudin (INRIA - Digiteo)\cr
Author of Scilab R adaptation: Sebastien Bihorel (<sb.pmlab@gmail.com>)\cr
Authors of bootstrap functions: Angelo Canty and Brian Ripley (originally by Angelo Canty for S)
}
\references{
There are many references explaining the concepts behind the functionality of this function. Among them are :

  Chang, Chih-Chung and Lin, Chih-Jen:\cr
  \emph{LIBSVM: a library for Support Vector Machines}\cr
  \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}

  Exact formulations of models, algorithms, etc. can be found in the
  document:\cr
  Chang, Chih-Chung and Lin, Chih-Jen:\cr
  \emph{LIBSVM: a library for Support Vector Machines}\cr
  \url{http://www.csie.ntu.edu.tw/~cjlin/papers/libsvm.ps.gz}

  More implementation details and speed benchmarks can be found on:\cr
  Rong-En Fan and Pai-Hsune Chen and Chih-Jen Lin:\cr
  \emph{Working Set Selection Using the Second Order Information for Training SVM}\cr
  \url{http://www.csie.ntu.edu.tw/~cjlin/papers/quadworkset.pdf}

  Spendley, W. and Hext, G. R. and Himsworth, F. R.\cr
  \emph{Sequential Application of Simplex Designs in Optimisation and Evolutionary Operation}\cr
  American Statistical Association and American Society for Quality, 1962\cr
  
  Nelder, J. A. and Mead, R.\cr
  \emph{A Simplex Method for Function Minimization}\cr
  The Computer Journal, 1965\cr
  
  C. T. Kelley\cr
  \emph{Iterative Methods for Optimization}\cr
  SIAM Frontiers in Applied Mathematics, 1999\cr
  
  A. C. Davison and D. V. Hinkley\cr
  \emph{Bootstrap Methods and Their Applications}\cr
  CUP, 1997\cr

  Booth, J.G., Hall, P. and Wood, A.T.A.\cr
  \emph{Balanced importance resampling for the bootstrap.}\cr
  Annals of Statistics, 21, 286-298, 1993\cr
  
  Davison, A.C. and Hinkley, D.V.\cr 
  \emph{Bootstrap Methods and Their Application}\cr
  Cambridge University Press, 1997\cr

  Efron, B. and Tibshirani, R.\cr 
  \emph{An Introduction to the Bootstrap}\cr 
  Chapman & Hall, 1993
}
\examples{
\dontrun{
data(iris)

irisClass <- iris[,5]
irisData  <- iris[,-5]

# Construct a classification ensemble with 100 classifiers and 100 bootstrap 
# iterations during optimisation

ens <- cfBuild(inputData = irisData, inputClass = irisClass, bootNum = 100, 
               ensNum = 100, parallel = TRUE, cpus = 4, type = "SOCK")

# The size of the classification ensemble
length(ens)

# List of attributes available for each classifier in the ensemble
attributes(ens[[1]])

# The test accuracy of one individual classifier in the ensemble 
round(ens[[1]]$testAcc, digits=2)

# The train accuracy of one individual classifier in the ensemble 
round(ens[[1]]$trainAcc, digits=2)

# All the test accuracies and the average test accuracy in the ensemble
getAcc(ens)$Test
getAvgAcc(ens)$Test

# All the train accuracies and the average train accuracy in the ensemble
getAcc(ens)$Train
getAvgAcc(ens)$Train

# Randomly generate test data to find out their classes using the generated ensemble
# 400 points are selected at random, which results in 100 samples (rows).

testMatr <-matrix(runif(400)*100, ncol = ncol(irisData))           

# Predict the classes of the data using the classifiers in the constructed ensemble

predRes <- cfPredict(ens, testMatr)
}
}
\keyword{classif}
\keyword{multivariate}
\keyword{models}
\keyword{nonlinear}
\keyword{optimize}
