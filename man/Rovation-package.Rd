\name{Rovation-package}
\alias{Rovation-package}
\docType{package}
\title{R Interface to the Ovation Scientific Data Management System}
\description{A set of utility allowing the user to access and modify an Ovation database (http://physionconsulting.com/web/Ovation.html)}
\details{
\tabular{ll}{Package: \tab Rovation\cr
Type: \tab Package\cr
Title: \tab R Interface to the Ovation Scientific Data Management System\cr
Version: \tab 0.0.1\cr
Date: \tab 2012-05-08\cr
Author: \tab Romain Franconville\cr
Depends: \tab rJava,tcltk\cr
SystemRequirements : \tab Java\cr
Maintainer: \tab Romain Franconville <franconviller@janelia.hhmi.org>\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr}
}
\author{Romain Franconville}

\keyword{ package }

\examples{
\dontrun{  ## Load the Rovation library
library('Rovation')

## Open a DataContext
context <- newDataContext('<connection file>, '<user name>')

## Get top-level objects from context.
# Note that rJava (the R-Java bridge) uses $ instead of . to represent method calls.
# So context.getProjects() in Java becomes context$getProjects() in R.
projs <- context$getProjects()
projs[[1]]$getPurpose()

## Plot a response
# You can use as.list to convert a Java array to an R array
exps<-as.list(projs[[1]]$getExperiments())
epochs <- as.list(exps[[1]]$getEpochsIterable())
epochs[[1]]
responses <- as.list(epochs[[1]]$getResponseIterable())
rData <- as.list(responses[[1]]$getFloatingPointData())
plot(seq(1,length(rData)),rData)


## Run a query and iterate the result
# Because next is an R keyword, use the iteratorNext method in Rovation to get the next object
# from an iterator
itr <- context$query(editQuery())
while(itr$hasNext()){epoch <- iteratorNext(itr)}
