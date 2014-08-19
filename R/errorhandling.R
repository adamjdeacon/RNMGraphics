
RNMGraphicsWarning <- function(msg = "Warning!\n", call = NULL)
{
	if(!is.null(call))
		cat(paste("In call: ", call[1],"(", paste(call[-1], collapse = ","), ")\n"))
	warning(simpleWarning(msg, call))
}


#' Generates a formal exception, and prints the sequence of calls that generated this expression.  This function
#' is meant to provide custom error handling for this package.
#' @title Generate an exception and message
#' @param msg Message 
#' @param call (optional).  Call information for function that generated exception (i.e. from match.call)
#' @return None
#' @author Mango Solutions
#' @keywords error
#' @export
#' @noRd

RNMGraphicsStop <- function(msg = "Error!\n",  call = NULL)
{
	if(!is.null(call))
		cat(paste("In call: ", call[1],"(", paste(call[-1], collapse = ","), ")\n"))
	dump.frames()
	cat(paste(names(head(last.dump, -1)), collapse = " >> \n "), "\n")
	
	stop(simpleError(msg, call))
}

RNMGraphicsStopifnot <-function(condition, msg = NULL, call = NULL)
{
	if(condition) return()
	if(is.null(msg))
		msg <- paste("The following condition failed:", deparse(substitute(condition))) 
	RNMGraphicsStop(msg, call = call)
}

assertClass <- function(object, targetClass)
{
	if(!inherits(object, targetClass))
	{
		msg <- paste(deparse(substitute(object)), "is not of class", targetClass)
		RNMImportStop(msg)
	}
	invisible(NULL)
}
