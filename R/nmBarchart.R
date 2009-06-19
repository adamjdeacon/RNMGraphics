# 
# $Rev$
# $LastChangedDate$
# $LastChangedRevision$
###############################################################################

#' Generates a categorical barchart of a set of categorical variables against another one.
#' @name nmBarChart
#' @title NONMEM categorical barchart
#' @param obj NMProblem or data.frame object
#' @param xVars Categorical variables on the x-axis 
#' @param yVars Categorical variables on the y-axis
#' @param xLab x-axis label
#' @param yLab y-axis label
#' @param titles Individual plot titles
#' @param ... additional parameters passed to the barchart function
#' @return Multitrellis class object
#' @author fgochez
#' @keywords hplot

nmBarChart <- function(obj, xVars, yVars, xLab = NULL, xRotAngle = 0, yLab = NULL, titles = NULL, addLegend = TRUE 
					   ,xBin = Inf ,...)
{
	RNMGraphicsStop("Not implemented for this class!", call = match.call())
}

setGeneric("nmBarChart")

nmBarChart.NMBasicModel <- function(obj, xVars, yVars, xLab = NULL, xRotAngle = 0 , yLab = NULL, titles = NULL, 
		addLegend = TRUE, xBin = Inf, ...)
{
	callList <- as.list(match.call())
	callList$obj <- nmData(obj)
	graphSubset(callList$obj) <- graphSubset(obj)
	do.call(nmBarChart, callList[-1])
}

# TODO: add bVars?
# TODO: test xBin

nmBarChart.data.frame <- function(obj, xVars, yVars, xLab = NULL, xRotAngle = 0, yLab = NULL, titles = "", 
		addLegend = TRUE, xBin = Inf, ...)
{
	xVars <- CSLtoVector(xVars)
	RNMGraphicsStopifnot(length(xVars) == 1, "Currently not accepting more than one x variable\n")
	yVars <- CSLtoVector(yVars)
	obj <- applyGraphSubset(obj)
	# bin the x variable if necessary
	if(length(unique(obj[[xVars]])) > xBin) {
		obj <- addDerivedCategorical(obj, xVars, paste(xVars, "BINNED", sep = "."), breaks = xBin, binType = "counts")
		xVars <- paste(xVars, "BINNED", sep = ".")
	}
	# take all combinations of x variables against y variables
	varCombos <- as.matrix(expand.grid(yVars, xVars))
	numCombos <- nrow(varCombos)

	if(!is.null(xLab))
		xLab <- rep(CSLtoVector(xLab), length.out = numCombos)
	else
		xLab <- varCombos[,2]
	if(!is.null(yLab))
		yLab <- rep(CSLtoVector(yLab), length.out = numCombos)
	else
		yLab <- varCombos[,1]
	plotList <- vector(mode = "list", length = nrow(varCombos))
	graphParams <- getAllGraphParams()
	for(i in seq_len(nrow(varCombos)))
	{
		currentX <- varCombos[i,2]
		currentY <- varCombos[i,1]
		#if(is.numeric(obj[,currentX]))

		tab <- table(obj[[currentX]], obj[[currentY]])
		# tab[,1] <- tab[,1] / sum(tab[,1]); tab[,2] <- tab[,2]
		tab <- sweep(tab, 1, rowSums(tab), "/")
		if(addLegend) key <- list(title = getVarLabel(currentY), 
					points = FALSE, rectangles = TRUE, space = "right", cex = 0.7)
		else key <- NULL
		scales <- list( x = list(rot = xRotAngle), y = list())
		plotList[[i]] <- 	
				barchart(tab, xlab = xLab[i], ylab = yLab[i], horizontal = FALSE, scales = scales,
							par.settings = mapTopar.settings(graphParams), 
					auto.key = key, main = titles, outer = TRUE, ...) # end with
	}
	
	result <- multiTrellis(plotList, RNMGraphics:::stdGridDims( numCombos, 3))
	result
	
}

setMethod("nmBarChart", signature(obj = "data.frame"), nmBarChart.data.frame)
setMethod("nmBarChart", signature(obj = "NMBasicModel"), nmBarChart.NMBasicModel)