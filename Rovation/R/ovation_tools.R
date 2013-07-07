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

NewUrl <- function(path)
{
    ### Returns a new file:// java.net.URL with the given path. Path may be absolute
    ### or relative to the current working directory.
    
    file <- .jnew("java/io/File", path)
    
    return(file$toURI()$toURL())
    ### Newly created java.net.URL
}

Vector2Set <- function(vec)
{
    ### Converts an R vector to a java.util.Set. Equal objects are only represented once in the resulting set, even
    ### if they are present more than once in the input vector.
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

Vector2List <- function(vec)
{
  ###Converts an R vector to a java.util.List.
  
  
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

EditQuery <- function(expression.tree=.jnull(class="com/physion/ebuilder/expression/ExpressionTree"))
{
    ### Edit an Ovation query using the GUI query editor
    
    ExpressionBuilder <- J("com/physion/ebuilder/ExpressionBuilder")
    rv <- ExpressionBuilder$editExpression(expression.tree)
    if(rv$status == ExpressionBuilder$RETURN_STATUS_OK)
        return(rv$expressionTree)
    else
        return(expression.tree)
}


Datetime <- function(year,month,day,hour=0,minute=0,second=0,millisecond=0,timezone=character())
{
    ### Construct an Ovation timestamp from date components.
    
    if (length(timezone)==0) {
        timezone <- .jcall("org/joda/time/DateTimeZone","Lorg/joda/time/DateTimeZone;",method="getDefault")
    } else {
        timezone <- .jcall("org/joda/time/DateTimeZone","Lorg/joda/time/DateTimeZone;",method="forID",timezone)
    }
    
    return(.jnew("org/joda/time/DateTime",as.integer(year),as.integer(month),as.integer(day),as.integer(hour),as.integer(minute),as.integer(second),as.integer(millisecond),timezone))
}

IteratorNext <- function(iterator)
{
    ### Wrapper for applying the "next" method to an iterator, because
    ### "next" is a protected word in R
    
    return(.jrcall(iterator,"next"))
      }

List2Map <- function(l)
{
    ### Converts an R list to a Java map
    
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
    ### A Java map
}

NumericData <- function(name, vec, units, sampling.rate, sampling.rate.units)
{
    ### Constructs an empty NumericData object from an R vector
    
    return(.jnew("ovation.NumericData")$addData(name, vec, units, sampling.rate, sampling.rate.units))
}

WaitForPendingUploads <- function(context, timeout.seconds = 600)
{
    ### Waits for all pending uploads from context to complete

    TimeUnit <- J("java.util.concurrent.TimeUnit")    

    fs <- context$getFileService()
    
    return(fs$waitForPendingUploads(.jlong(timeout.seconds), TimeUnit$SECONDS))
}
