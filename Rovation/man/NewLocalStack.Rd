\name{NewLocalStack}
\alias{NewLocalStack}
\title{Creates a new Ovation database stack for testing}
\usage{
  NewLocalStack(name)
}
\arguments{
  \item{name}{database name}
}
\value{
  new us.physion.ovation.test.util.TestUtils instance
}
\description{
  A testing stack creates a complete, isolated Ovation
  database and in-memory "cloud" storage provider. A local
  stack should be used for testing only, and should be
  cleaned up with \code{localStack$cleanUp} when done.
}

