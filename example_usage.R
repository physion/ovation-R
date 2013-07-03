### Here is an example of some basic Ovation usage from R

## Load the Rovation library
library('Rovation')

## Open a DataContext
context <- NewDataContext('<user email>')

## Get top-level objects from context.
# Note that rJava (the R-Java bridge) uses $ instead of . to represent method calls.
# So context.getProjects() in Java becomes context$getProjects() in R.
projs <- as.list(context$getProjects())
projs[[1]]$getPurpose()

## Plot a response
# You can use as.list to convert a Java array to an R array
exps<-as.list(projs[[1]]$getExperiments())
epochs <- as.list(exps[[1]]$getEpochs())
epochs[[1]]
responses <- as.list(epochs[[1]]$getResponses())
rData <- as.list(responses[[1]]$getFloatingPointData())
plot(seq(1,length(rData)),rData)


## Run a query and iterate the result
# Because next is an R keyword, use the nextJavaIt method in Rovation to get the next object
# from an iterator
itr <- context$query(editQuery())
while(itr$hasNext())
	epoch <- IteratorNext(itr)
end
