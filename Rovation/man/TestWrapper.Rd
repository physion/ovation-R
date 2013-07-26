\name{TestWrapper}
\alias{TestWrapper}
\title{Wrapps a callable in a LocalStack}
\usage{
  TestWrapper(name, test.fn)
}
\arguments{
  \item{name}{database name}

  \item{test.fn}{test callable}
}
\description{
  The test function should take a \code{DataContext} as its
  only parameter. It will be passed an authenticated
  \code{DataContext} for the local stack. The local stack
  is automatically cleaned up after the function exits.
}

