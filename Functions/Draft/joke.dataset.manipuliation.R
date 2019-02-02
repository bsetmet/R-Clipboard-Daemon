#Jokes dataset for jokes
#joke.data <- read.csv(file="https://raw.githubusercontent.com/amoudgl/short-jokes-dataset/master/shortjokes.csv",header=TRUE,sep=",")
#OR Use a Subset of clean jokes here:
joke.data <- read.csv(file="https://raw.githubusercontent.com/amoudgl/short-jokes-dataset/master/data/reddit-cleanjokes.csv",header=TRUE,sep=",")

sample.simple <- function(x,n=1,fReplace=TRUE) {
# modified from https://stackoverflow.com/a/27652752
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,n,replace=fReplace))
  }
}

# this method returns a list (vector) from the original data frames variable
# see https://stackoverflow.com/a/7070330
joke.list <- as.vector(joke.data[['Joke']])
# this method returns a dataframe with one variable (column)
# joke.list <- joke.data['Joke'] 

# Prints a random joke(s) from the list
print(sample.simple(joke.list))
print(sample.simple(joke.list,5))

# This randomly unsorts the list of jokes (Jumbles)
joke.list.jumbled <-sample.simple(
	joke.list,length(joke.list),
	fReplace=FALSE
)
print(joke.list.jumbled)

joke.list.ordered <- sort(
	joke.list.jumbled, 
	na.last=NA, 
	decreasing=FALSE, 
	method="radix"
)
print(joke.list.ordered)