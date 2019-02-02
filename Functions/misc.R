# Usage is 
# source("\\\\snnsvr122\\c105\\C105.4\\Budget\\Budget Nexus\\For Bill\\SFSSCC\\Draft\\misc.R")

check.dir.exists <- function(dir,fCreateIfMissing=TRUE){
  if (!(dir.exists(dir))){	
    dir.create(dir, recursive=TRUE)
  }
  return(dir.exists(dir))
}

install.package <- function(pkg, fRequirePkg = FALSE){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  logDir <-paste(getwd(),"Logs", sep="/")
  if (check.dir.exists(logDir)){
    logPath <- paste(logDir, "ErrorLog_Check_Packages.csv",sep="/")
  }
  #[TODO] replace btf.log.errors function
  if (length(new.pkg)){
    tryCatch({
      install.results <- install.packages(new.pkg, dependencies=TRUE)
      btf.log.errors(install.results,"Install.Results",pkg,logPath)
    }, warning = function(war) {btf.log.errors(war,"WARNING:Install",pkg,logPath)
    }, error = function(err) {btf.log.errors(err,"ERROR:Install",pkg,logPath)})
  }
  if (fRequirePkg) {
    tryCatch({
      require.results <- sapply(pkg, require, character.only=TRUE)
      btf.log.errors(require.results,"Require.Reulsts",pkg,logPath)
    }, warning = function(war) {btf.log.errors(war,"WARNING:Require",pkg,logPath)
    }, error = function(err) {btf.log.errors(err,"ERROR:Require",pkg,logPath)})
  }
}

log.installed.packages <- function (
  logDir=paste(getwd(),"Logs", sep="/"),
  fOpenAfterLogging=FALSE)
{
  # for directions, look at:
  # https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/
  if (check.dir.exists(logDir)){  
    logPath <- paste(logDir, paste0("installed.packages.",R.Version()$major,".",R.Version()$minor,".",Sys.info()["nodename"],".csv"),sep="/")
    packages <- installed.packages(
      lib.loc = NULL, priority = NULL,
      noCache = FALSE, fields = NULL,
      subarch = .Platform$r_arch
    )
    write.csv(packages, logPath)
    if (fOpenAfterLogging){
      shell.exec((logPath))
    }
  } else {
    print(paste0("Unable to find or create directory ",logDir))
  }
}

open.folder <- function (dirPath=getwd()) {
  install.package("stringr",TRUE)
  platform <- paste0(Sys.info()["sysname"])
  dirPath <- str_replace_all(dirPath,"/","\\")
  if (platform=="Windows") {
    shell.exec(dirPath)
    # [TODO]Errors with
    # \\snnsvr122/c105/C105.4/Budget/Budget Nexus/For Bill/R/R Code is not accessible. You might not have permission to use this network resource. Contact the administrator of this server to find out if you have access permissions.
    # The parameter is incorrect.
    # need to perform Find replace from / to \\ strings??? with gsub()
   
    return(TRUE)
  }
  if (platform=="Unix") {
    system(paste0("xdg-open ",dirPath))
    # This really works only for linux, not Mac OS
    # For mac it should be: system2("open", dir)
    return(TRUE)
  }
}

# Custom functions
# just not the builtin is.null function
is.not.null <- function(x) ! is.null(x)

# can't simply check for the not of is.na, as is.na(x) objects return the contents instead of the expected boolean
is.not.na <- function(x) ! (paste(x,"",sep="") == "NA")

# Remove Trailing Character
# (this could be a function)
str.remove.trailing <- function(strTarget,trailing.character="/"){
  if ((substr(strTarget,nchar(strTarget),nchar(strTarget))) == trailing.character){
    return(strtrim(strTarget,nchar(strTarget)-1))
  } else {
    return(strTarget)
  }
}

