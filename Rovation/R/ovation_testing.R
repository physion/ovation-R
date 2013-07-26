

#' Creates a new Ovation database stack for testing
#' 
#' A testing stack creates a complete, isolated Ovation database and in-memory "cloud" storage provider.
#' A local stack should be used for testing only, and should be cleaned up with \code{localStack$cleanUp} when
#' done.
#' 
#' @return new us.physion.ovation.test.util.TestUtils instance
#' @param name database name
#' @export
NewLocalStack <- function(name) {
  test.utils <- new(J("us.physion.ovation.test.util.TestUtils"))
  local.stack <- test.utils$makeLocalStack(new(J("us.physion.ovation.api.OvationApiModule")),
                                           name,
                                           name,
                                           name)
  
  return(local.stack)
}

#' Wrapps a callable in a LocalStack
#' 
#' The test function should take a \code{DataContext} as its only parameter. It will be passed
#' an authenticated \code{DataContext} for the local stack. The local stack is automatically
#' cleaned up after the function exits.
#'
#' @param name database name
#' @param  test.fn test callable
#' @export
TestWrapper <- function(name, test.fn) {
  stack <- NewLocalStack(name)
  tryCatch(test.fn(stack$getAuthenticatedDataStoreCoordinator()$getContext()),
           finally=stack$cleanUp())
}