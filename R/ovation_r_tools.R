NewDataContext <- function(email)
{
    ### Returns a DataContext object after prompting for password
    
    require(tcltk)  # The prompt uses the tcltk library
    tt<-tktoplevel()
    tkwm.geometry(tt,"+700+400")
    tktitle(tt) <- "ovation.io"
    Password <- tclVar("") 
    entry.Password <-tkentry(tt,width="20",textvariable=Password,show="*") 
    tkgrid(tklabel(tt,text="Please enter your password.")) 
    tkgrid(entry.Password)
    OnOK <- function() 
    { 
        tkdestroy(tt)   
        Password <<- tclvalue(Password)     
    } 
    OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
    tkfocus(entry.Password)
    tkbind(entry.Password, "<Return>",OnOK) 
    tkgrid(OK.but) 
    tkwait.window(tt)
    Ovation <- .jnew("ovation/Ovation")
    dataContext <- Ovation$connect(email,Password)
    return(dataContext)
    ### A reference to a DataContext object
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
