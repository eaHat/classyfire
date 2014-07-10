setwd("/Library/WebServer/CGI-Executables/newPack")
getwd()

package.skeleton(name="classyfire", code_files=c("bootstrapping.R","initChecks.R", "parallelFunc.R", "runRadialComplex.R", "runRBF.R", "parPerm.R", "plotFunc.R", "statFunc.R"))


------------------



cd /Library/WebServer/CGI-Executables/newPack/
R CMD BUILD classyfire
R CMD CHECK classyfire
R CMD REMOVE classyfire