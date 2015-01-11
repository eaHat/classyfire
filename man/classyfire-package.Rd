\name{classyfire-package}
\alias{classyfire-package}
\alias{classyfire}
\docType{package}
\title{
Robust multivariate classification using highly optimised SVM ensembles
}
\description{
The aim of the \code{classyfire} package is to improve the quality of multivariate classification projects by making a state-of-the-art multivariate classification workflow available to everyone. Classyfire achieves this by providing powerful functions which automate as much of the classifier building and testing as possible. However, to avoid these functions becoming impenetrable black boxes, detailed information is provided about how these functions work, and full access is provided to the internals of all classifiers that are produced. 
}
\details{
\tabular{ll}{
Package: \tab classyfire\cr
Type: \tab Package\cr
Version: \tab 0.1-2\cr
Date: \tab 2015-01-11\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Adapted functionality by Eleni Chatzimichali (ea.chatzimichali@gmail.com)\cr \cr
Author of the SVM functions: David Meyer (<David.Meyer@R-project.org>)\cr
(based on C/C++-code by Chih-Chung Chang and Chih-Jen Lin)\cr
Author of Scilab neldermead module: Michael Baudin (INRIA - Digiteo)\cr
Author of Scilab R adaptation: Sebastien Bihorel (<sb.pmlab@gmail.com>)\cr
Authors of bootstrap functions: Angelo Canty and Brian Ripley (originally by Angelo Canty for S)
}
\references{
There are many references explaining the concepts behind the functionality of this package. Among them are :

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
\keyword{ package }

