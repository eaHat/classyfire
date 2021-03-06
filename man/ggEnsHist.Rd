\name{ggEnsHist}
\alias{ggEnsHist}
\title{
Ensemble Histograms
}
\description{
The \code{ggEnsHist} function generates a histogram of the ensemble results as generated by \code{\link{cfBuild}}.
}
\usage{
ggEnsHist(ensObj, density = FALSE, percentiles = FALSE, mean = FALSE, median = FALSE)
}
\arguments{
  \item{ensObj}{The classification ensemble (in the form of an R list) as generated by \code{\link{cfBuild}}}
  \item{density}{Boolean value, by default equal to \code{FALSE}. If \code{density = FALSE}, the histogram depicts frequencies, the counts component of the result. Instead, for \code{density = TRUE}, probability densities are plotted.}
  \item{percentiles}{Boolean value, by default equal to \code{FALSE}. If \code{percentiles = TRUE}, the upper and lower percentiles of the distribution are depicted in the plot.}
  \item{mean}{Boolean value, by default equal to \code{FALSE}. If \code{mean = TRUE}, the mean of the distribution is depicted in the plot.}
  \item{median}{Boolean value, by default equal to \code{FALSE}. If \code{median = TRUE}, the median of the distribution is depicted in the plot.}
}
\seealso{
  \code{
    \link{ggPermHist}
  }
}
\examples{
\dontrun{
data(iris)

irisClass <- iris[,5]
irisData  <- iris[,-5]

ens <- cfBuild(irisData, irisClass, bootNum = 100, ensNum = 100, parallel = TRUE, 
               cpus = 4, type = "SOCK")

# Basic histogram of the test accuracies in the ensemble
ggEnsHist(ens)

# Density plot of the test accuracies in the ensemble
ggEnsHist(ens, density = TRUE)

# Density plot that highlights additional descriptive statistics
ggEnsHist(ens, density = TRUE, percentiles=TRUE)
ggEnsHist(ens, density = TRUE, percentiles=TRUE, mean=TRUE)
ggEnsHist(ens, density = TRUE, percentiles=TRUE, median=TRUE)
}
}
\keyword{hplot}
