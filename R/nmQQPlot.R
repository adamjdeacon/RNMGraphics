# $Rev$
# $LastChangedDate$

#' Generates a qq-plot (for the normal distribution) from one or more NONMEM variables
#' @name nmQQNorm
#' @title NONMEM qqplot 
#' @param obj NMRun, NMProblem, or data.frame object
#' @param vars Vector or comma-seperated list of variables to plot 
#' @param bVars Vector or comma-seperated list of "by" variables
#' @param titles plot titles
#' @param xLabs x-axis labels
#' @param yLabs y-axis labels
#' @param addGrid unused
#' @param qqLine logical flag.  Should a reference line be added?
#' @param yAxisScales One of "same" "free" "sliced". How panel y axis are scaled in relation to each other 
#' @param ... additional parameters to pass to qqmath
#' @return Multi-trellis class object
#' @author fgochez
#' @keywords hplot

nmQQNorm <- function(obj, vars, bVars = NULL, iVar = "ID", titles = "", xLabs = "normal", yLabs, 
		addGrid = TRUE, qqLine = TRUE, yAxisScales = c("same","free","sliced"), layout = NULL, maxPanels = NULL,
		maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{
	RNMGraphicsStop("Not implemeneted for this class\n")
}

nmQQNorm.NMRun <-function(obj, vars, bVars = NULL, iVar = "ID", titles = "",
		xLabs = "normal", yLabs, addGrid = TRUE, qqLine = TRUE, yAxisScales = c("same","free","sliced"), 
		layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmScatterMatrix, x[-1])
}

nmQQNorm.NMProblem <- function(obj, vars, bVars = NULL, iVar = "ID", titles = "", xLabs = "normal", yLabs, 
		addGrid = TRUE, qqLine = TRUE, yAxisScales = c("same","free","sliced"), layout = NULL, maxPanels = NULL,
		maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{
	dataSet <- nmData(obj, subProblemNum = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(nmScatterMatrix, x[-1])
	
	RNMGraphicsStop("Not implemeneted for this class\n")
}


setGeneric("nmQQNorm")



nmQQNorm.data.frame <- function(obj, vars, bVars = NULL, iVar = "ID", titles = "",
			xLabs = "normal", yLabs, addGrid = TRUE, qqLine = TRUE, yAxisScales = c("same","free","sliced"), 
			layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
{   
	vars <- CSLtoVector(vars)
	obj <- applyGraphSubset(obj)
	# we now filter variables that do not have more than one level
	# TODO: UNITTEST
	hasSeveralValues <- sapply(vars, function(n) length(unique(obj[,n])) > 1)
	if(!all(hasSeveralValues))
		RNMGraphicsWarning("Several columns have been detected with only a single value, dropping\n")
	vars <- vars[hasSeveralValues]
	uncollapsedVars <- vars
	if(length(vars) == 0)
		RNMGraphicsStop("None of the supplied variables had more than one value", match.call())
	vars <- paste(vars, collapse = "+")
	dataSet <- obj

	numCombos <- 1
	if(length(maxPanels) > 0) layout <- NULL
	# ensure that maxPanels is numeric, even if empty
	else maxPanels <- numeric(0)
	if(missing(yLabs)) yLabs <- vars
	

	plotFormulas <- paste(" ~ ", vars)
	
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		temp <- processTrellis(dataSet, bVars, maxLevels = maxTLevels, exempt = iVar)
		dataSet <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "*"), sep = "|") 
	}
	plotList <- vector(mode = "list", length = numCombos)
	graphParams <- getAllGraphParams()
	additions <- c("qqLine" = qqLine)
	if (length(uncollapsedVars) > 1 | length(bVars) > 0) scales <- list(relation=yAxisScales)
	else scales <- list()

	plt <- with(graphParams,
		qqmath(as.formula(plotFormulas), main = titles, data = dataSet, 
		panel = panel.nmQQNorm, additions = additions, xlab = xLabs, ylab = yLabs, 
		outer = TRUE,
		scales = scales,
		par.settings = list(par.xlab.text = axis.text, 
		par.ylab.text = axis.text, par.main.text = title.text, 
		plot.symbol = plot.symbol, strip.background = strip.bg), 
		 strip = getStripFun(), 
		..., layout = layout))
	multiTrellis(list(plt), maxPanels = maxPanels)
}

setMethod("nmQQNorm", signature(obj = "data.frame") , nmQQNorm.data.frame)


panel.nmQQNorm <- function(x, additions, ...)
{
	reflineOpts <- getGraphParams("refline")
	panel.qqmath(x,...)
	if(additions["qqLine"])
		panel.qqmathline(x, col = reflineOpts$col, lwd = reflineOpts$lwd, lty = reflineOpts$lty)
	
}
