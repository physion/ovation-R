\name{IteratorNext}
\alias{IteratorNext}
\title{Calls the \code{next} method on a Java iterator.}
\usage{
  IteratorNext(iterator)
}
\arguments{
  \item{iterator}{Java iterator}
}
\value{
  result of iterator$next()
}
\description{
  Because "next" is a reserved keyword in R, calling
  \code{iterator$next()} fails.
}

