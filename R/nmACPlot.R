

#' nmACPlot plots a given variable in a data set against itself "lagged" by one time step.
#' @name  nmACPlot
#' @title NONMEM autocorrelation plot
#' @param obj NMProblem or data.frame object
#' @param var Name of the variable to be plotted
#' @param tVar Name of the "time" variable
#' @param iVar Identifier variable
#' @param bVars "by" variables 
#' @param gVars grouping variable
#' @param titles Main title
#' @param xLabs x-axis label
#' @param yLabs y-axis label
#' @param extraSubset Not used at the moment
#' @param addGrid logical flag.  Should a grid be added?
#' @param problemNum The problem required for a \code{NMRun} object. 
#' @param subProblems The sub problem of a run with simulations.
#' @param ... Extra parameters passed to nmScatterPlot
#' @return Obejct of class multiTrellis
#' @author Mango Solutions
#' @keywords hplot
#' @exportMethod nmACPlot

nmACPlot <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, 
		problemNum = 1, subProblems = 1, ...)
{
	RNMGraphicsStop("Not implemented for this class at the moment")
}


setGeneric("nmACPlot")

nmACPlot.NMRun <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, 
		problemNum = 1, subProblems = 1, ...)
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmACPlot, x[-1])
}

setMethod("nmACPlot", signature(obj = "NMRun"), nmACPlot.NMRun)


nmACPlot.NMProblem <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, 
		problemNum = 1, subProblems = 1, ...)
{
	
	funcCall <- as.list(match.call())[-1]
	dataSet <- nmData(obj, subProblemNum = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	funcCall$obj <- dataSet
	do.call(nmACPlot, funcCall)
}

setMethod("nmACPlot", signature(obj = "NMProblem"), nmACPlot.NMProblem)

nmACPlot.data.frame <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, 
		problemNum = 1, subProblems = 1, ...)
{
	
	## include conversion from comma seperated list to vector to allow for handling of 
	## comma seperated list case as permitted in all other functions
	var <- CSLtoVector(var, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(var) == 1, "Must have only one variable\n")
	# lag while preserving the ID structure
	# only one variable allowed at the moment
	var <- var[1]
	
	# remove any IDs with only one entry
	IDcounts <- table(obj[[iVar]])
	IDStoRemove <- names(IDcounts)[IDcounts < 2]
	obj <- obj[!(obj[[iVar]] %in% IDStoRemove), ] 
	
	laggedVar <- paste(var, ".LAGGED", sep = "")
	lagRecords <- function(x)
	{
		if(nrow(x) < 2)
			return(NULL)
		x <- x[order(x[[tVar]]),]
		y <- rbind(  tail(x, -1), rep(NA, length.out = ncol(x)))
		
		colnames(y) <- replace(colnames(y), which(colnames(x) == var),laggedVar)
		y[[var]] <- x[[var]]
	#	y[nrow(y), var] <- NA
		y
		
	}
	obj.lagged <- by(obj[c(var, tVar)], list(obj[[iVar]]), lagRecords )
	obj.lagged <- do.call(rbind, obj.lagged); obj[[laggedVar]] <- obj.lagged[[laggedVar]]
	obj[[var]] <- obj.lagged[[var]]
	# obj <- na.omit(obj)
	nmScatterPlot(obj, xVars = laggedVar, yVars = var,
			bVars = bVars, gVars = gVars, xLab = xLabs, yLab = yLabs,
			addGrid = addGrid, extraSubset = extraSubset, titles = titles, ...)
}

setMethod("nmACPlot", signature(obj = "data.frame"), nmACPlot.data.frame)