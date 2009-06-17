# TODO: Add comment
#  $Rev$
#  $LastChangeDate: $
# Author: fgochez
###############################################################################


defaultGraphSubset <- function()
{
	return(.RNMGraphicsEnv$defaultSubset)
}

setDefaultGraphSubset <- function(sub)
{
	.RNMGraphicsEnv$defaultSubset <- sub
}
	# TODO: make this a generic function later
	
"graphSubset<-" <- function(x, value)
{
	attr(x, "graphSubset") <- value
	x
}

graphSubset <- function(x)
{
	attr(x, "graphSubset")
}

applyGraphSubset <- function(obj, sub = NULL)
{
	if(is.null(sub))
		return(obj)

	for(x in sub)
	{
		res <- try(subset(obj, eval(parse(text = x)) ), silent = TRUE)
		if(!inherits(res, "try-error")) obj <- res
		
	}
	return(obj)	
}