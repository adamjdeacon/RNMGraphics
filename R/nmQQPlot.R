
#' Generates a qq-plot (for the normal distribution) from one or more NONMEM variables
#' @title NONMEM qqplot 
#' @param obj NMRun, NMProblem, or data.frame object
#' @param vars Vector or comma-separated list of variables to plot 
#' @param bVars Vector or comma-separated list of "by" variables
#' @param iVar Identifier variable
#' @param titles Main title
#' @param xLabs x-axis label
#' @param yLabs y-axis label
#' @param addGrid unused
#' @param qqLine logical flag.  Should a reference line be added?
#' @param layout Numeric vector giving the number of columns, rows and pages in a multipanel display. 
#' @param maxPanels Maximum number of panels that should appear on each page of a graph. 
#' @param maxTLevels If a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned.
#'                      If a character vector or a vector of length greater than one, the explicit breakpoints.
#' @param problemNum  The problem required for a \code{NMRun} object. 
#' @param subProblems The sub problem of a run with simulations. 
#' @param xAxisScaleRelations One of "same" "free" "sliced". How panel x axes are scaled in relation to each other
#' @param yAxisScaleRelations One of "same" "free" "sliced". How panel x axes are scaled in relation to each other
#' @param ... additional parameters to pass to qqmath
#' @return Multi-trellis class object containing the plot
#' @author Mango Solutions
#' @examples
#' \dontrun{ 
#' nmQQNorm(ChickWeight.df,"weight", bVar =  "Diet", xAxisScaleRelations="free")
#' }
#' @keywords hplot
#' @exportMethod nmQQNorm


nmQQNorm <- function(obj, vars, bVars = NULL, iVar = "ID", titles = "", xLabs = "normal", yLabs, 
		addGrid = TRUE, qqLine = TRUE, yAxisScaleRelations = c("same","free","sliced"), layout = NULL, maxPanels = NULL,
		maxTLevels = Inf, problemNum = 1, subProblems = 1, xAxisScaleRelations = c("same","free","sliced"), ...)
{
	RNMGraphicsStop("Not implemeneted for this class\n")
}

nmQQNorm.NMRun <-function(obj, vars, bVars = NULL, iVar = "ID", titles = "",
		xLabs = "normal", yLabs, addGrid = TRUE, qqLine = TRUE, yAxisScaleRelations = c("same","free","sliced"), 
		layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, 
        xAxisScaleRelations = c("same","free","sliced"), ...)
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmQQNorm, x[-1])
}

nmQQNorm.NMProblem <- function(obj, vars, bVars = NULL, iVar = "ID", titles = "", xLabs = "normal", yLabs, 
		addGrid = TRUE, qqLine = TRUE, yAxisScaleRelations = c("same","free","sliced"), layout = NULL, maxPanels = NULL,
		maxTLevels = Inf, problemNum = 1, subProblems = 1, xAxisScaleRelations = c("same","free","sliced"),  ...)
{
	dataSet <- nmData(obj, subProblemNum = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(nmQQNorm, x[-1])
	
}

setGeneric("nmQQNorm")



nmQQNorm.data.frame <- function(obj, vars, bVars = NULL, iVar = "ID", titles = "",
			xLabs = "normal", yLabs, addGrid = TRUE, qqLine = TRUE, yAxisScaleRelations = c("same","free","sliced"), 
			layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, xAxisScaleRelations = c("same","free","sliced"),  ...)
{   
	## include removeEmpty option to prevent empty string errors
	## include error handling if not enough variables are provided
	vars <- CSLtoVector(vars, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(vars) > 0, "At least one variable must be provided to create this plot.\n")
	
	vars <- CSLtoVector(vars)
	obj <- applyGraphSubset(obj)
	# we now filter variables that do not have more than one level

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
		temp <- processTrellis(dataSet, bVars, maxLevels = maxTLevels, exemptColumns = iVar)
		dataSet <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "*"), sep = "|") 
	}
	plotList <- vector(mode = "list", length = numCombos)
	graphParams <- getAllGraphParams()
	additions <- c("qqLine" = qqLine)
	

	
	# if (length(uncollapsedVars) > 1 | length(bVars) > 0) 
    scales <- list( y = list(relation= match.arg(yAxisScaleRelations)), x = list(relation = match.arg(xAxisScaleRelations)))	

	plt <- with(graphParams,
		qqmath(as.formula(plotFormulas), main = titles, data = dataSet, 
		panel = panel.nmQQNorm, additions = additions, xlab = xLabs, ylab = yLabs, 
		outer = TRUE,
		scales = scales,
		par.settings = list(par.xlab.text = axis.text, 
		par.ylab.text = axis.text, par.main.text = title.text, 
		plot.symbol = plot.symbol, strip.background = strip.bg), 
		 strip = getStripFun(),graphParams = graphParams, 
		..., layout = layout ) )
	multiTrellis(list(plt), maxPanels = maxPanels)
}

setMethod("nmQQNorm", signature(obj = "data.frame") , nmQQNorm.data.frame)
setMethod("nmQQNorm", signature(obj = "NMProblem") , nmQQNorm.NMProblem)
setMethod("nmQQNorm", signature(obj = "NMRun") , nmQQNorm.NMRun)

panel.nmQQNorm <- function(x, additions, graphParams, ...)
{
	reflineOpts <- graphParams$"refline"
	panel.qqmath(x,...)
	if(additions["qqLine"])
		panel.qqmathline(x, col = reflineOpts$col, lwd = reflineOpts$lwd, lty = reflineOpts$lty)
	
}
