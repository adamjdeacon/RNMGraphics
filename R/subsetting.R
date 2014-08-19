
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

#' @description NONMEM data objects and data frames can have "graphical subsets" associated to them. 
#' These subsets will be applied whenever a graphing function is called on the object. 
#' This functionality is intended to facilitate the plotting of "standard" subsets, i.e. subsets without dosing information. 
#' @details Subsets assigned to an object are obtained using \code{graphSubset}. Subsets will be a set a vector of strings 
#' which are converted to R expressions and then executed. They can be assigned using \code{graphSubset<-}.
#' @name Graphical Subsetting
#' @aliases graphSubset graphSubset<-
#' @title Change and Retrieve Graphical Subsets
#' @param x An object of class \code{NMRun}, \code{NMProblem} or \code{data.frame}.
#' @param value A vector of (character) subsets.
#' @return For \code{graphSubset}, a character vector representing the subsets to be applied to \code{x} before plotting. 
#' For \code{graphSubset<-}, the updated object, i.e. with additional or removed subsets.
#' @author Mango Solutions
#' @keywords environment
#' @export
#' @examples 
#' \dontrun{
#' x <- importNm("TestData1.ctl", "TestData1.lst", 
#'   path  = system.file(package = "RNMImport", "unittests/testdata/TestRun")) 
#' graphSubset(x) <- c("MDV != 1", "EVID == 0", "AMT <= 0") 
#' graphSubset(x)
#' }
#' 

"graphSubset<-" <- function(x, value)
{
	attr(x, "graphSubset") <- value
	x
}

#' @name Graphical Subsetting
#' @aliases graphSubset graphSubset<-
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