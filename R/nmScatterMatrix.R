

#' Generates a scatterplot matrix of a set of variables from a PK/PD dataset
#' @name nmScatterMatrix
#' @title NONMEM scatter-plot matrix
#' @param obj object of class NMProblem, NMRun or data.frame
#' @param vars Character vector or comma seperated list of variable names to plot
#' @param bVars character vector or comma-seperated list of trellis variables
#' @param iVar Identifier variable
#' @param addLoess Logical flag. Should a loess smoother curve be added to the scatter-plots? 
#' @param title Plot's main title
#' @param layout Numeric vector giving the number of columns, rows and pages in a multipanel display. 
#' @param maxPanels Maximum number of panels that should appear on each page of a graph.  Overrides \code{layout} 
#' @param maxTLevels If a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned. 
#'        If a character vector or a vector of length greater than one, the explicit breakpoints.
#' @param problemNum The problem required for a \code{NMRun} object. 
#' @param subProblems The sub problem of a run with simulations.
#' @param ... Additional parameters to \code{splom}.
#' @return An object of class multiTrellis 
#' @author Mango Solutions
#' @keywords hplot
#' @exportMethod nmScatterMatrix

nmScatterMatrix <- function(obj, vars,bVars = NULL, iVar = "ID",  addLoess = FALSE, title ="", 
		layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{
	RNMGraphicsStop("Not implemented for this class yet!")
}


setGeneric("nmScatterMatrix")

nmScatterMatrix.NMRun <- function(obj, vars,bVars = NULL, iVar = "ID",  addLoess = FALSE, title ="", 
		layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmScatterMatrix, x[-1])
}

nmScatterMatrix.NMProblem <- function(obj, vars,bVars = NULL, iVar = "ID",  addLoess = FALSE, title ="", 
		layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{
	dataSet <- nmData(obj, subProblems = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(nmScatterMatrix, x[-1])
}

# TODO: ability to add L curve

nmScatterMatrix.data.frame <- function(obj, vars,bVars = NULL, iVar = "ID",  
		addLoess = FALSE, title = "", layout = NULL, maxPanels = NULL, maxTLevels = Inf, 
		problemNum = 1, subProblems = 1, ...)
{
	## Add error handling to give informative error if less than two variables are provided
	if(length(vars) < 2){
		if(length(vars)==1){
			splitVars <- CSLtoVector(vars, removeEmpty = TRUE)
			if(length(splitVars)<2){
				stop("At least two variables must be selected to create this graphic")
			}
		} else{
			stop("At least two variables must be selected to create this graphic")
		}	
	}
	
	# Use the removeEmpty option to prevent error with empty strings
	vars <- CSLtoVector(vars, removeEmpty = TRUE)
	vars <- paste("'", vars, "'", sep = "")
	numCombos <- length(vars)
	if(length(maxPanels) > 0) layout <- NULL
	# ensure that maxPanels is numeric, even if empty
	else maxPanels <- numeric(0)
	
	obj <- applyGraphSubset(obj)
	
	# only one allowed at the moment
	plotFormulas <- paste(" ~ obj[c(", paste(vars, collapse = ","), ")]")
	
	# standard logic - process the by variables
	# this will add the by variables to the lattice formula, and convert to factors as necessary
	
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		temp <- processTrellis(obj, bVars, maxLevels = maxTLevels, exemptColumns = iVar)
		obj <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "+"), sep = "|") 
	}	
	
	graphParams <- getAllGraphParams()
	stripfunc <- getStripFun()
	plt <- splom(as.formula(plotFormulas), data = obj, main = title, panel = panel.nmScatterMatrix, addLoess = addLoess, 
			par.settings = list(par.xlab.text = graphParams$"axis.text", 
					par.ylab.text = graphParams$"axis.text", par.main.text = graphParams$title.text,
					plot.symbol = graphParams$plot.symbol, strip.background = graphParams$strip.bg),
			layout = layout, strip = stripfunc ,...)
	
	multiTrellis(list(plt), maxPanels = maxPanels)
		
}

setMethod("nmScatterMatrix", signature(obj = "data.frame"), nmScatterMatrix.data.frame)
setMethod("nmScatterMatrix", signature(obj = "NMRun"), nmScatterMatrix.NMRun)
setMethod("nmScatterMatrix", signature(obj = "NMProblem"), nmScatterMatrix.NMProblem)

# scatter matrix panel function
# This simply adds a loess smoother in a special manner with error handling

panel.nmScatterMatrix <- function(x,y, addLoess = FALSE, ...)
{
	panel.xyplot(x,y,...)
	if(addLoess)
	{
		loessopt <- getGraphParams("loess.line")
		loessTry <- try(with(loessopt, panel.loess(x, y, col = col, lwd = lwd) ), silent = TRUE)
		if(inherits(loessTry, "try-error")) RNMGraphicsWarning("A call to panel.loess failed, so the smoother will be omitted.\n")
	}
}