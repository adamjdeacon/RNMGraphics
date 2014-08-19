# SVN revision: $Rev$
# Date of last change: $LastChangedDate$
# Last changed by: $LastChangedBy$
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# This code may just alias RNMImport functions later

defaultGraphSubset <- function()
{
	return(.RNMGraphicsEnv$defaultSubset)
}

setDefaultGraphSubset <- function(sub)
{
	.RNMGraphicsEnv$defaultSubset <- sub
}
	# TODO: make this a generic function later

#' assigns to \code{"graphSubset"} attribute
#' @title Calculate Max Panels
#' @param x
#' @param value
#' @return x
#' @author fgochez
#' @keywords graph
#' @export

"graphSubset<-" <- function(x, value)
{
	attr(x, "graphSubset") <- value
	x
}

#' returns the value of attribute \code{"graphSubset"}
#' @title Calculate Max Panels
#' @param x
#' @return character vector
#' @author fgochez
#' @keywords graph
#' @export

graphSubset <- function(x)
{
	attr(x, "graphSubset")
}

applyGraphSubset <- function(obj, sub = graphSubset(obj))
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