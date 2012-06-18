NewDataContext <- function
### Returns a DataContext object after prompting for password
  (connection,
   ### Connection to the database
   login
   ### User login
   ){
  require(tcltk)  # The prompt uses the tcltk library
  tt<-tktoplevel()
  tkwm.geometry(tt,"+700+400")
  tktitle(tt) <- "Log in OvationDB"
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
  dataContext <- Ovation$connect(connection,login,Password)
  return(dataContext)
  ### A reference to a DataContext object
}

editQuery <- function
### Edit an Ovation query using the GUI query editor
(expressionTree=.jnull(class="com/physion/ebuilder/expression/ExpressionTree"))
{
	ExpressionBuilder <- J("com/physion/ebuilder/ExpressionBuilder")
	rv <- ExpressionBuilder$editExpression(expressionTree)
	if(rv$status == ExpressionBuilder$RETURN_STATUS_OK)
		return(rv$expressionTree)
	else
		return(expressionTree)
}


datetime <- function
### Construct an Ovation timestamp from date components.
(year,month,day,hour=0,minute=0,second=0,millisecond=0,timezone=character())
  {
     if (length(timezone)==0){
       timezone <- .jcall("org/joda/time/DateTimeZone","Lorg/joda/time/DateTimeZone;",method="getDefault")
       }else{
         timezone <- .jcall("org/joda/time/DateTimeZone","Lorg/joda/time/DateTimeZone;",method="forID",timezone)}
       
    .jnew("org/joda/time/DateTime",as.integer(year),as.integer(month),as.integer(day),as.integer(hour),as.integer(minute),as.integer(second),as.integer(millisecond),timezone)
     ### A DateTime object
        }

iteratorNext <- function
### Wrapper for applying the "next" method to an iterator, because "next" is a protected word in R
(iterator){
    .jrcall(iterator,"next")
   ### Returns the next object the iterator is refering to 
}

list2map <- function
### Converts an R list to a Java map
(l
 ### A list
 ){
  keys <- names(l)
  m <- .jnew("java/util/HashMap",length(l))
  lapply(keys,function(k){
    value <- l[[k]]
## The put method requires wrapper classes input, so we need to convert beforehand (character is automatically converted to String by R, and vectors to java arrays).
    if(is.logical(value)){
      value <- .jnew("java/lang/Boolean",value)
    }
    
    if(is.raw(value)){
      value <- .jbyte(value)
    }
    
    if(is.double(value) & length(value)==1){
      value <- .jnew("java/lang/Float",value)
    }
    
    if(is.integer(value) & length(value)==1){
      value <- .jnew("java/lang/Integer",value)
    }
    m$put(k,value)
  })

  return(m)
  ### A Java map         
       }
