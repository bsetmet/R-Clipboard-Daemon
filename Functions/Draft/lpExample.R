setwd("G:/My Drive/Bsetmet/Finance/Projects/Development/R_Profiles/Asus")

#install.packages("lpSolveAPI")
library(lpSolveAPI)
#lpData <- data.frame(x=c(1,1,1,2,2,3,4,4),y=c(5,6,7,8,7,5,6,5),w=c(1,2,3,4,5,6,7,8))
#write.csv(lpData,"lpData.csv")

OptimizationType<-"max"
lpData<-read.csv("lpData.csv")

#Make model
lpModel <- make.lp(ncol=nrow(lpData))
set.type(lpModel, columns=1:nrow(lpData), type = c("integer"))

#this is the objective function
#The first column is always the objective function
objectiveFunction <- lpData[, 1]
set.objfn(lpModel, objectiveFunction)

lp.control(lpModel,sense=OptimizationType)



nRows<-nrow(lpData)
zero<-seq(from=0, to=0, length.out=nRows)
add.constraint(lpModel, xt=(zero),
               indices=c(1:nRows), rhs=1, type=">=")





add.constraint(lpModel, xt=c(1,1,1), #xt specifies which rows of the LP
               indices=c(1,2,3), rhs=1, type="<=")
add.constraint(lpModel, xt=c(1,1), #xt specifies which rows of the LP
               indices=c(4,5), rhs=1, type="<=")
add.constraint(lpModel, xt=c(1,1), #xt specifies which rows of the LP
               indices=c(7,8), rhs=1, type="<=") #x's in dataframe rows 7 & 8 are both '4'

#Add constraints to limit Y values from repeating
add.constraint(lpModel, xt=c(1,1,1), #xt specifies which rows of the LP
               indices=c(1,6,8), rhs=1, type="<=") #Y's in df rows 1,6 & 8 are all '5'
add.constraint(lpModel, xt=c(1,1), #xt specifies which rows of the LP
               indices=c(2,7), rhs=1, type="<=") #Y's in dataframe rows 2&7 are both '6'
add.constraint(lpModel, xt=c(1,1), #xt specifies which rows of the LP
               indices=c(3,5), rhs=1, type="<=") #y's in dataframe rows 3&5 are both '7'

solve(lpModel)
get.objective(lpModel) #20
get.variables(lpModel)
#[1] 0 0 1 1 0 1 1 0
#This tells you that from d you pick rows: 3,4,6 & 7 in your optimal solution.

#If you want to look at the full formulation:
rownames1 <- paste("OneX", c(1,2,4), sep="_")
rownames2 <- paste("OneY", c(5,6,7), sep="_")
colnames<- paste("pick_",c(1:8), sep="")
dimnames(lpModel) <- list(c(rownames1, rownames2), colnames)
print(lpModel)

#write it to a text file
write.lp(lpModel,filename="max_w.lp")

rm(list=ls())
