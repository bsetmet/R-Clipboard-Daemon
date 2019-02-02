source.r.files.from.dir <- function(functionsDir) {
  errorLogPath <- paste(functionsDir,"Logs","ErrorLog_load_custom_functions.csv",sep="\\")
  if (dir.exists(functionsDir)){
    # source our functions
    sourceFiles <- dir(functionsDir, pattern = "(.[.]r|.[.]R)$")
    for (file in sourceFiles){
      print(paste("Sourcing File:",paste(functionsDir, file, sep="\\")))
      tryCatch({
        source(paste(functionsDir, file, sep="\\"))
      }, warning = function(war) {
          logLine <- paste('"',paste(Sys.time(),file,"WARNING",war,sep='","'),'"',sep="") 
          print(logLine)
          if (!file.exists(errorLogPath)){
            try(write('"Time","File","Category","Details"',file=errorLogPath,append=TRUE))
          }
          try(write(logLine,file=errorLogPath,append=TRUE))
      }, error = function(err) {
        logLine <- paste('"',paste(Sys.time(),file,"ERROR",err,sep='","'),'"',sep="") 
        print(logLine)
        if (!file.exists(errorLogPath)){
          try(write('"Time","File","Category","Details"',file=errorLogPath,append=TRUE))
        }
        try(write(logLine,file=errorLogPath,append=TRUE))
      })
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}
log.currently.installed.packages<-function(strMyProjectPath){
  strLogDir <- paste(strMyProjectPath,"Logs",sep="\\")
  strFunctionDir <- paste(strMyProjectPath,"Functions",sep="\\")
  # sequence of events is build log path if missing, source.r.files.from.dir expects it, then load functions prior to log.installed.packages
  if (dir.exists(strMyProjectPath)) {
  	if (!dir.exists(strLogDir)) {
  		dir.create(strLogDir)
  	}
  	if (dir.exists(strFunctionDir)) {
  		source.r.files.from.dir(strFunctionDir)
  	}
  	log.installed.packages(strLogDir)	
  	# TODO: Some function to check (and install missing) for all expected packages from a maintained list...
  }
}

