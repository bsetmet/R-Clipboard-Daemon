# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
# https://gist.github.com/smithdanielle/9913897/raw/cc303deefff8885d4d3e0d94986f8409fd2427ad/check.packages.r
install.package.if.missing <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
log.installed.packages <- function (functionLogPath){
	# for directions, look at:
	#https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/
	packages<-installed.packages(
		lib.loc = NULL, priority = NULL,
		noCache = FALSE, fields = NULL,
		subarch = .Platform$r_arch
	)
	fName<-paste(functionLogPath, paste("list_ofinstalled_packages_",Sys.info()["nodename"],".csv",sep=""), sep="\\")
	write.csv(packages, fName)
}
