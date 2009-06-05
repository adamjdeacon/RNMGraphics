# Autocorrelation plots
# 
# $Rev$
# $LastChangedDate$
#
# Author: fgochez
###############################################################################


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
#' @param ... Extra parameters passed to nmScatterPlot
#' @return Obejct of class multiTrellis
#' @author fgochez
#' @keywords hplot

nmACPlot <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, ...)
{
	RNMGraphicsStop("Not implemented for this class at the moment")
}

setGeneric("nmACPlot")

nmACPlot.NMBasicModel <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, ...)
{
# TODO: make the "match.call" approach work
#	browser()
#	funcCall <- match.call()
#	obj <- nmData(obj)
#	callList <- do.call("c", as.list(funcCall))
#	callList$obj <- obj
	
	funcCall <- as.list(match.call())[-1]
	dataSet <- nmData(obj)
	funcCall$obj <- dataSet
	do.call(nmACPlot, funcCall)
	#nmACPlot(obj, var, tVar, iVar, bVars, gVars,titles, xLabs, yLabs, extraSubset, addGrid,...)
}
setMethod("nmACPlot", signature(obj = "NMBasicModel"), nmACPlot.NMBasicModel)
nmACPlot.data.frame <- function(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL,
		titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, ...)
{
	
	# lag while preserving the ID structure
	# only one variable allowed at the moment
	var <- var[1]
	laggedVar <- paste(var, ".LAGGED", sep = "")
	lagRecords <- function(x)
	{
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
	nmScatterPlot(obj, xVar = laggedVar, yVar = var,
			bVars = bVars, gVars = gVars, xLab = xLabs, yLab = yLabs,
			addGrid = addGrid, extraSubset = extraSubset, titles = titles, ...)
}

setMethod("nmACPlot", signature(obj = "data.frame"), nmACPlot.data.frame)