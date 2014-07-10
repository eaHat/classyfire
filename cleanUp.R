# Clean everything 

rm(list = ls(all = TRUE))
graphics.off()


detach("package:e1071")
detach("package:neldermead")
detach("package:boot")
detach("package:Rmpi")
detach("package:optSVMs")