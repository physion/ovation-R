
#' Builds a new DataContext (connection) to the Ovation database
#' 
#' \code{NewDataContext} connects to the local Ovation database using the provided user email
#' and entered password. After establishing a connection to the database, a new \code{DataContext}
#' is created and returned.
#' 
#' @param email registered ovation.io email
#' @return new \code{DataContext}
#' @export
NewDataContext <- function(email)
{
  ### Returns a DataContext object after prompting for password
  
  require(tcltk)  # The prompt uses the tcltk library
  tt<-tktoplevel()
  tkwm.geometry(tt, "+700+400")
  tktitle(tt) <- "ovation.io"
  Password <- tclVar("") 
  entry.Password <-tkentry(tt, width="20", textvariable=Password, show="*") 
  tkgrid(tklabel(tt,text="Please enter your password.")) 
  tkgrid(entry.Password)
  OnOK <- function() 
  { 
    tkdestroy(tt)   
    Password <<- tclvalue(Password)     
  } 
  OK.but <-tkbutton(tt,text="   OK   ", command=OnOK)
  tkfocus(entry.Password)
  tkbind(entry.Password, "<Return>",OnOK) 
  tkgrid(OK.but) 
  tkwait.window(tt)
  
  Ovation <- .jnew("us/physion/ovation/api/Ovation")
  
  dsc <- Ovation$connect(email, Password)
  
  return(dsc$getContext())
  ### A reference to a DataContext object
}

#' Creates a new \code{java.net.URL} from the given local path
#' 
#' @param path local file path (absolute or relative to the current working directory)
#' @return newly created \code{java.net.URL}
#' @export
NewUrl <- function(path)
{
  file <- .jnew("java/io/File", path)
  
  return(file$toURI()$toURL())
}

#' Creates a \code{java.util.Set} from an R vector
#' 
#' Equal objects are only represented once in the resulting set, even if they
#' are present more than once in the input vector.
#' 
#' @param vec R vector
#' @return \code{Set} with the contents of \code{vec}
#' @export
Vector2Set <- function(vec)
{
  HashSet <- J("java.util.HashSet")
  Double <- J("java.lang.Double")
  Integer <- J("java.lang.Integer")
  
  h <- new(HashSet)
  
  for(v in vec) {
    if(is.numeric(v)) {
      if(is.integer(v)) {
        value <- new(Integer, v)
      } else {
        value <- new(Double, v)  
      }
    } else {
      value <- v
    }
    
    h$add(value)
  }
  
  return (h)
}

#' Creates a \code{java.util.List} from an R vector
#' 
#' @param vec R vector
#' @return \code{List} with the contents of \code{vec}
#' @export
Vector2List <- function(vec)
{
  
  
  Double <- J("java.lang.Double")
  Integer <- J("java.lang.Integer")
  
  ArrayList <- J("java.util.ArrayList")
  l <- new(ArrayList)
  
  for(v in vec) {
    if(is.numeric(v)) {
      if(is.integer(v)) {
        value <- new(Integer, v)
      } else {
        value <- new(Double, v)  
      }
    } else {
      value <- v
    }
    
    l$add(value)
  }
  
  return (l)
}

#' Edits an Ovation query using the GUI query editor
#' 
#' @param expression.tree existing expression tree to edit (optional)
#' @return edited expression tree
EditQuery <- function(expression.tree=.jnull(class="com/physion/ebuilder/expression/ExpressionTree"))
{
  ExpressionBuilder <- J("com/physion/ebuilder/ExpressionBuilder")
  rv <- ExpressionBuilder$editExpression(expression.tree)
  if(rv$status == ExpressionBuilder$RETURN_STATUS_OK)
    return(rv$expressionTree)
  else
    return(expression.tree)
}


#' Constructs an Ovation timestamp from date components.
#'
#' @param year year
#' @param month month of year
#' @param day day of month
#' @param hour hour of day (optional; default = 0)
#' @param minute minute of hour (optional; default = 0)
#' @param second second of minute (optional; default = 0)
#' @param millisecond milliseconds of second (optional; default = 0)
#' @param timezone time zone (optional; default = local time zone)
#' @return created \code{org.joda.time.DateTime} instance
#' @export
Datetime <- function(year,month,day,hour=0,minute=0,second=0,millisecond=0,timezone=character())
{
  if (length(timezone)==0) {
    timezone <- .jcall("org/joda/time/DateTimeZone","Lorg/joda/time/DateTimeZone;",method="getDefault")
  } else {
    timezone <- .jcall("org/joda/time/DateTimeZone","Lorg/joda/time/DateTimeZone;",method="forID",timezone)
  }
  
  return(.jnew("org/joda/time/DateTime",as.integer(year),as.integer(month),as.integer(day),as.integer(hour),as.integer(minute),as.integer(second),as.integer(millisecond),timezone))
}

#' Calls the \code{next} method on a Java iterator.
#' 
#' Because "next" is a reserved keyword in R, calling \code{iterator$next()} fails.
#' 
#' @param iterator Java iterator
#' @return result of iterator$next()
#' @export
IteratorNext <- function(iterator)
{
  return(.jrcall(iterator,"next"))
}

#' Creates a \code{java.util.Map} from the contents of an R list
#' 
#' @param l R list
#' @return \code{Map} with the same keys and values as \code{l}
List2Map <- function(l)
{
  keys <- names(l)
  m <- .jnew("java/util/HashMap",length(l))
  lapply(keys,function(k) {
    value <- l[[k]]
    ## The put method requires wrapper classes input, so we need to convert beforehand (character is automatically converted to String by R, and vectors to java arrays).
    if(is.logical(value)) {
      value <- .jnew("java/lang/Boolean",value)
    }
    
    if(is.raw(value)) {
      value <- .jbyte(value)
    }
    
    if(is.double(value) & length(value)==1) {
      value <- .jnew("java/lang/Float",value)
    }
    
    if(is.integer(value) & length(value)==1) {
      value <- .jnew("java/lang/Integer",value)
    }
    
    m$put(k,value)
  })
  
  return(m)
}

#' Constructs an new Ovation \code{NumericData} object from an R vector
#' 
#' @param name data name
#' @param vec vector of numeric data contents
#' @param units data units
#' @param sampling.rate data sampling rate
#' @param sampling.rate.units data sampling rate units (e.g. "Hz")
#' @return newly constructed \code{NumericData}
#' @export
NumericData <- function(name, vec, units, sampling.rate, sampling.rate.units)
{
  return(.jnew("ovation.NumericData")$addData(name, vec, units, sampling.rate, sampling.rate.units))
}

#' Waits for pending file uploads
#' 
#' When data is added to the Ovation database, the data is uploaded to the Ovation.io cloud asynchronously.
#' This method waits the given timeout for all pending uploads to complete. File uploads are tied to the
#' \code{DataContext} of the entities that reference the data.
#' 
#' @param context DataContext to wait for
#' @param timeout.seconds wait timeout seconds
#' @return upload results
#' @export
WaitForPendingUploads <- function(context, timeout.seconds = 600)
{
  TimeUnit <- J("java.util.concurrent.TimeUnit")    
  
  fs <- context$getFileService()
  
  return(fs$waitForPendingUploads(.jlong(timeout.seconds), TimeUnit$SECONDS))
}
