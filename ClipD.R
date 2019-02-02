#Setup Project
rm(list=ls())
  
#strMyProjectPath <- paste(Sys.getenv("USERPROFILE"),"Documents","R","Projects","Basic Project Framework",sep="\\")
#strMyProjectPath <- paste("\\\\snnsvr122","c105","C105.4","Budget","Budget Nexus","For Bill","R\ Clipboard\ Daemon",sep="\\")
strMyProjectPath <- paste(Sys.getenv("USERPROFILE"),"AppData\\Local\\LocalWorking\\R\ Clipboard\ Daemon",sep="\\")
setwd(strMyProjectPath)

source(paste(strMyProjectPath,"Profile","initialize.R",sep="\\"))

# some houskeeping
Sys.setenv(R_HISTSIZE=99999)
options(keep.source.pkgs=TRUE)
options(repos=structure(c(CRAN="http://cloud.r-project.org/")))


#Select your mirror prior to running install.packages("something")
#Menue items Package -> Set CRAN Mirror
install.package.if.missing("RODBC") 
require(RODBC)
require(readr)
require(clipr)
require(lpSolve)
require(lpSolveAPI)
require(uuid)


# some random stuff
random.large.int <- floor(runif(10^5,min=-20,max=101))
hundredths <- c(1:100)/100
random.large.quantile.hundredths <- quantile(random.large.int,hundredths)
random.normal.distribution.large <- floor(rnorm(10^6,mean=50, sd=10))
random.normal.distribution.large <- 
	floor(
		rnorm(
			10^6,
			mean=80,
			sd=10
		)
	)

save.image("random.dat")

# custom quit function
save.and.quit <- function (fQuit = TRUE) {
	gc() # force garbage collection
	try(savehistory())
	strHistoryBackupFilePath <- paste0(
		"~/.",
		format(as.Date(Sys.Date(),"%y-%m-%d")),
		".Rhistory"
	)
	strWorkspaceBackupFilePath <- paste0(
		"~/.",
		format(as.Date(Sys.Date(),"%y-%m-%d")),
		".Rprofile"
	)
	try(savehistory(strHistoryBackupFilePath))
	try(save.image(strWorkspaceBackupFilePath))
	# We can verify that our daily backups have saved by 
	# checking list.files("~/",all.files=TRUE), 
	#or rather 
	if(!file.exists(strHistoryBackupFilePath)){
		# write to error log...
		print("Expected history backup file not created!")
	}	
	if(fQuit){
		q(save="yes")
	}
}

# We can read from the clipboard!!!
# select excel data and copy it, first cell second row contains folder path
x <- read_delim(clipboard(),"\t", col_names = FALSE)
print(x)
#dir(x[[1]][2])

# or use clipr for clipboard access
X <- read_clip_tbl()
Print(X)
#Write to clipboard
writeClipboard(c(c("test",4),c("Yes","No")))
#or use
write_clip(c(c("test",4),c("No","Yes")))


#Example Lineear progamming from https://www.r-bloggers.com/linear-programming-in-r/
# Load lpSolve
require(lpSolve)

## Set the coefficients of the decision variables -> C
C <- c(1:30)

# Create constraint martix B
A <- matrix(c(1:3600), nrow=120, byrow=TRUE)

# Right hand side for the constraints
B <- c(500, 200, 100, 1000)

# Direction of the constraints
constranints_direction  <- c("<=", "<=", "<=", ">=")

# Find the optimal solution
optimum <-  lp(direction="min",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("x_4p", "x_3p", "x_w") 
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total cost: ", optimum$objval, sep=""))

#################
#   Output      #
#################

# [1] 0
# x_4p x_3p  x_w 
# 420  580  161 

# "Total cost: 48680"

rm(optimum, constranints_direction, best_sol)



#-------------------------------------------------------------------------------
# Let's try to solve the problem again using lpSolveAPI

# Use lpSolveAPI
require(lpSolveAPI)

# Set 4 constraints and 3 decision variables
lprec <- make.lp(nrow = 4, ncol = 3)
# Set the type of problem we are trying to solve
lp.control(lprec, sense="min")
# Set type of decision variables
set.type(lprec, 1:3, type=c("integer"))

# Set objective function coefficients vector C
set.objfn(lprec, C)

# Add constraints
add.constraint(lprec, A[1, ], "<=", B[1])
add.constraint(lprec, A[2, ], "<=", B[2])
add.constraint(lprec, A[3, ], "<=", B[3])
add.constraint(lprec, A[4, ], ">=", B[4])

# Display the LPsolve matrix
lprec

# Solve problem
solve(lprec)

# Get the decision variables values
get.variables(lprec)
# Get the value of the objective function
get.objective(lprec)

# Note that the default boundaries on the decision variable are c(0, 0, 0) and c(Inf, Inf, Inf)
get.bounds(lprec)

# Boundaries can be set with following function
#lpSolveAPI::set.bounds()

#################
#   Output      #
#################

# [1] 420 580 161
# [1] 48680
