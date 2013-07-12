\name{NewDataContext}
\alias{NewDataContext}
\title{Builds a new DataContext (connection) to the Ovation database}
\usage{
  NewDataContext(email)
}
\arguments{
  \item{email}{registered ovation.io email}
}
\value{
  new \code{DataContext}
}
\description{
  \code{NewDataContext} connects to the local Ovation
  database using the provided user email and entered
  password. After establishing a connection to the
  database, a new \code{DataContext} is created and
  returned.
}

