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
#' @param ... additional parameters to pass to qqmath
#' @return Multi-trellis class object
#' @author fgochez
#' @keywords hplot

nmQQNorm <- function(obj, vars, bVars = NULL, titles = "", xLabs = "normal", yLabs, 
		addGrid = TRUE, qqLine = TRUE, ...)
{
	RNMGraphicsStop("Not implemeneted for this class\n")
}

setGeneric("nmQQNorm")

panel.nmQQNorm <- function(x, additions, ...)
{
	reflineOpts <- getGraphParams("refline")
	panel.qqmath(x,...)
	if(additions["qqLine"])
		panel.qqmathline(x, col = reflineOpts$col, lwd = reflineOpts$lwd, lty = reflineOpts$lty)
	
}

nmQQNorm.data.frame <- function(obj, vars, bVars = NULL, titles = "",
			xLabs = "normal", yLabs, addGrid = TRUE, qqLine = TRUE, ...)
{
	vars <- paste(CSLtoVector(vars), collapse = "+")
	dataSet <- obj

	numCombos <- 1
	
	if(missing(yLabs)) yLabs <- vars
	
	# repeatVars(c("titles", "qqLine", xLabs, yLabs), list(titles, qqLine, xLabs, yLabs), 
	#		length.out = numCombos)

	plotFormulas <- paste(" ~ ", vars)
	
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		dataSet <- coerceToFactors(dataSet, bVars)
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "*"), sep = "|") 
	}
	plotList <- vector(mode = "list", length = numCombos)
	graphParams <- getAllGraphParams()
	additions <- c("qqLine" = qqLine)
	plt <- with(graphParams,
		qqmath(as.formula(plotFormulas), main = titles, data = dataSet, 
		panel = panel.nmQQNorm, additions = additions, xlab = xLabs, ylab = yLabs, 
		outer = TRUE,
		par.settings = list(par.xlab.text = axis.text, 
		par.ylab.text = axis.text, par.main.text = title.text, 
		plot.symbol = plot.symbol, strip.background = strip.bg), # strip = strip, 
		...))
	multiTrellis(list(plt))
}

setMethod("nmQQNorm", signature(obj = "data.frame") , nmQQNorm.data.frame)