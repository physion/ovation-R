\name{Vector2Set}
\alias{Vector2Set}
\title{Creates a \code{java.util.Set} from an R vector}
\usage{
  Vector2Set(vec)
}
\arguments{
  \item{vec}{R vector}
}
\value{
  \code{Set} with the contents of \code{vec}
}
\description{
  Equal objects are only represented once in the resulting
  set, even if they are present more than once in the input
  vector.
}

