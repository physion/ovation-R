\name{WaitForPendingUploads}
\alias{WaitForPendingUploads}
\title{Waits for pending file uploads}
\usage{
  WaitForPendingUploads(context, timeout.seconds = 600)
}
\arguments{
  \item{context}{DataContext to wait for}

  \item{timeout.seconds}{wait timeout seconds}
}
\value{
  upload results
}
\description{
  When data is added to the Ovation database, the data is
  uploaded to the Ovation.io cloud asynchronously. This
  method waits the given timeout for all pending uploads to
  complete. File uploads are tied to the \code{DataContext}
  of the entities that reference the data.
}

