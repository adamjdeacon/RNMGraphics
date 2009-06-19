# $Rev$
# $LastChangedDate$


#' craetes histograms of one or more NONMEM variables
#' @name nmHistogram
#' @title NONMEM histogram
#' @param obj The object from which data will be plotted (NMRun, NMProblem or data.frame)
#' @param vars Variables from which to generate a histogram (character vector or comma seperate string of names)
#' @param bVars “Trellis” variables on which to split data.  
#' @param refLine Controls addition of a reference line to the histogram(s).  Use is self-explanatory.
#' @param type Determines the style of y-axis that is used for the plot (percentages, frequencies, or proportions)
#' @param addDensity Logical flag.  Should a density estimate be plotted? Only relevant for type = "density"
#' @param titles Plot titles
#' @param xLabs x axis labels
#' @param extraSubset Currently unused
#' @param addGrid Currently unused
#' @param nint Number of intervals for the creation of X axis bars.  It functions identically to the nint parameter of the histogram function from the lattice package.
#' @param breaks Control the calculation of breakpoints for the histogram.  It functions identically to the breaks parameter of the histogram function from the lattice package.
#' @param ... Additional parameters passed to panel.histogram 
#' @return An object of class multiTrellis
#' @author fgochez
#' @keywords hplot

nmHistogram <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
				 addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, 
				 maxTLevels = Inf,  problemNum = 1, subProblems = 1, ...)
{
	RNMGraphicsStop("Not implemented for this class at the moment")
}

nmHistogram.NMRun <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
		addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, 
		maxTLevels = Inf,  problemNum = 1, subProblems = 1, ...)
{

	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmHistogram, x[-1])
}

# TODO: handle simulated data

nmHistogram.NMProblem <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
							addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL,
							maxTLevels = Inf,  problemNum = 1, subProblems = 1, ...)
{
	dataSet <- nmData(obj, subProblemNum = subProblems )
	
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(nmHistogram, x[-1])
	
}

nmHistogram.data.frame <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
							addGrid = TRUE, nint = 12, breaks, layout = NULL,
							maxPanels = NULL,maxTLevels = Inf,  problemNum = 1, subProblems = 1, ...)
{
	if(!(is.element(refLine, c("none", "mean", "median"))))
		RNMGraphicsStop("Reference line parameter not valid!")
	if(!(is.element(type, c("count", "percent", "density"))))
		RNMGraphicsStop("Type parameter not valid!")
	if(type != "density")
		addDensity <- FALSE
	if(length(maxPanels) > 0) layout <- NULL
	# ensure that maxPanels is numeric, even if empty
	else maxPanels <- numeric(0)
	
	vars <- paste(CSLtoVector(vars), collapse = "+")
	numCombos <- length(vars)
	titles <- rep(titles,numCombos)
	
	plotFormulas <- paste(" ~ ", vars)
	if(missing(xLabs)) xLabs <- vars
	repeatVars(c("titles", "xLabs"), list(titles, xLabs), length.out = numCombos)
	dataSet <- applyGraphSubset(obj)
	
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		temp <- processTrellis(dataSet, bVars, maxLevels = maxTLevels, exempt = iVar)
		dataSet <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "*"), sep = "|")
	}
	plotList <- vector(mode = "list", length = numCombos)
	stripfn = getStripFun()
	exp <- quote(histogram(as.formula(plotFormulas), main = titles, data = dataSet, xlab = xLabs, 
					par.settings = list(plot.polygon = getGraphParams("histogram"), par.xlab.text = getGraphParams("axis.text"), 
					par.ylab.text = getGraphParams("axis.text"),
					strip.background = getGraphParams("strip.bg")), 
					refLine = refLine, type = type, addDensity = addDensity, panel = panel.nmHistogram, outer = TRUE, 
					strip = stripfn, nint = nint, ...))
	
	if(!missing(breaks))
		exp$breaks <- breaks	
	
	plt <- eval(exp)
	multiTrellis(list(plt), maxPanels = maxPanels)
}

panel.nmHistogram <- function(x, refLine, addDensity, ...)
{
	refVal <- switch(refLine,
			"none"   = NULL,
			"mean"   = mean(x, na.rm = TRUE),
			"median" = median(x, na.rm = TRUE))
	panel.histogram(x, ...)
	if(addDensity)
		panel.densityplot(x, col = "black", ...)
	reflineOpts <- getGraphParams("refline")
	panel.abline(v = refVal, col = reflineOpts$col, lwd = reflineOpts$lwd, 
			lty = reflineOpts$lty, ...)
	
}


setGeneric("nmHistogram")
setMethod("nmHistogram", signature(obj = "NMProblem"), nmHistogram.NMProblem)
setMethod("nmHistogram", signature(obj = "NMRun"), nmHistogram.NMRun)
setMethod("nmHistogram", signature(obj = "data.frame"), nmHistogram.data.frame)