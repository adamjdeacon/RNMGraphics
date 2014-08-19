
#' Create a custom dotplot of one or more continuous variables against a categorical variable
#' @name nmDotPlot
#' @title NONMEM dot plot
#' @param obj An object of class \code{NMRun}, \code{NMProblem}, or \code{data.frame}. The object from which data will be plotted.
#' @param factVar Character variable representing the factor variable. 
#' @param contVar Character vector or comma separated string specifying continuous variables.
#' @param bVars Trellis variables, specified as characters (or \code{NULL}, which is the default).
#' @param iVar Subject identifier variable (single string or NULL).
#' @param gVar Grouping variable
#' @param title Plot main title
#' @param xLabs X-axis label
#' @param yLabs Y-axis label
#' @param layout Numeric vector giving the number of columns, rows and pages in a multipanel display.  is overwritten by maxPanels.
#' @param maxPanels Maximum number of panels that should appear on each page of a graph.
#' @param addLegend Logical flag.  Should legends be added?
#' @param maxTLevels If a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned.
#'                      If a character vector or a vector of length greater than one, the explicit breakpoints.
#' @param problemNum The problem required for a \code{NMRun} object. 
#' @param subProblems The sub problem of a run with simulations.
#' @param maxFactPerPanel Max fact per panel.
#' @param xAxisPlotStyle "data" or "cont" or "cat".
#' @param yAxisScaleRelations Y-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}. 
#' @param xAxisScaleRelations X-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param ... Additional parameters to \code{dotplot}. 
#' @return Multitrellis class object containing the plot.
#' @author Mango Solutions
#' @examples
#' \dontrun{ 
#'  Theoph.df <- as.data.frame(Theoph)
#'  nmDotPlot( Theoph.df, factVar = "Subject", contVar = "Dose" )
#' }
#' @keywords hplot
#' @exportMethod nmDotPlot

nmDotPlot <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf, maxFactPerPanel = Inf, problemNum = 1, subProblems = 1,
        xAxisScaleRelations = c("same", "free", "sliced"), yAxisScaleRelations = c("same", "free", "sliced"),
		xAxisPlotStyle = c("data", "cont", "cat"),
		...)   
{
	RNMGraphicsStop("Not implemented for this class yet \n")	
}

nmDotPlot.NMRun <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf, maxFactPerPanel = Inf, problemNum = 1, subProblems = 1,
        xAxisScaleRelations = c("same", "free", "sliced"), yAxisScaleRelations = c("same", "free", "sliced"),
		xAxisPlotStyle = c("data", "cont", "cat"),
		...)   
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmDotPlot, x[-1])
}

nmDotPlot.NMProblem  <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf,  maxFactPerPanel = Inf,  
		problemNum = 1, subProblems = 1,
        xAxisScaleRelations = c("same", "free", "sliced"), 
        yAxisScaleRelations = c("same", "free", "sliced"),
		xAxisPlotStyle = c("data", "cont", "cat"), ...)
{
	dataSet <- nmData(obj, sumProblemNum = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(nmDotPlot, x[-1])
}


nmDotPlot.data.frame <- function(obj, factVar, contVar, bVars = NULL,iVar = "ID", gVar = "NULL", 
					title = NULL, xLabs = NULL, yLabs = NULL,layout = NULL, maxPanels = numeric(0),	
					addLegend = TRUE, maxTLevels = Inf,  maxFactPerPanel = Inf,  
					problemNum = 1, subProblems = 1, 
                    xAxisScaleRelations = c("same", "free", "sliced"), 
                    yAxisScaleRelations = c("same", "free", "sliced"), 
					xAxisPlotStyle = c("data", "cont", "cat"), ...)   
{

	## include removeEmpty option to prevent empty string errors
	## include error handling if not enough variables are provided
	contVar <- CSLtoVector(contVar, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(contVar) > 0, "At least one variable must be provided to contVar to create this plot.\n")
	factVar <- CSLtoVector(factVar, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(factVar) > 0, "At least one variable must be provided to factVar to create this plot.\n")
	
	contVars <- paste(CSLtoVector(contVar), collapse = "+")
	factVars <- paste(CSLtoVector(factVar), collapse = "+")

	xAxisPlotStyle <- match.arg(xAxisPlotStyle)
	if (xAxisPlotStyle == "cat") {
		contVars <- paste("as.factor(", contVars, ")")
		factVars <- paste("as.numeric(", factVars, ")")
	}
	
	plotFormula <- paste(factVars, "~", contVars)
        
    
	obj <- applyGraphSubset(obj, graphSubset(obj))
	if(!is.null(bVars))
    {
        bVars <- CSLtoVector(bVars)
    }
	
    # initialize the scales 
    
	scales <- list(x = list( relation = match.arg(xAxisScaleRelations) ), 
            y = list( relation = match.arg(yAxisScaleRelations)  ))
	
    # force factVar to be a factor
	
    obj <- coerceToFactors(obj, factVar)
	
	# group the factor variable if there are too many levels, leading to "crammed" axis labels
	if(length(unique(obj[[factVar]])) > maxFactPerPanel)
	{
		
		# sort by factor first, if possible
		obj <- obj[order(as.numeric(obj[[factVar]])),]
		
		x <- .factorTrellis(obj[[factVar]], maxFactPerPanel)
		obj[[paste(factVar, "GRP", sep = ".")]] <- x
		bVars <- c(bVars, paste(factVar, "GRP", sep = "."))
		
        # need to free y axis relations to take advantage of this
		
		scales$y$relation <- "free"
		
	}
	
	if(!is.null(bVars))
	{
		
		temp <- processTrellis(obj, bVars, maxLevels = maxTLevels, exemptColumns = c(iVar, paste(factVar, "GRP", sep = ".")))
		obj <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
    
        plotFormula <- paste( plotFormula, paste(bVars, collapse = "+"), sep = "|" )
	}
	
    if(is.null(xLabs))
    {
        xLabs <- contVars
    }
    
    if(is.null(yLabs))
    {
        yLabs <- factVars
    }
    
	graphParams <- getAllGraphParams()
	dot.symbol <- graphParams$plot.symbol
	
	par.settings <- with(graphParams, list(
					dot.symbol = dot.symbol, superpose.symbol = superpose.symbol,				
					par.xlab.text = axis.text, par.ylab.text = axis.text,
					par.main.text = title.text, plot.line = plot.line,
					add.line = refline, strip.background = graphParams$strip.bg, 
					layout.widths = layout.widths, layout.heights = layout.heights))

	if(length(maxPanels) > 0) layout <- NULL
	# ensure that maxPanels is numeric, even if empty
	else maxPanels <- numeric(0)
    	
    auto.key <- if(addLegend) list(title = getVarLabel(gVar), cex=.7, rows=10,space="right") else NULL
		
	dPlot <- dotplot(as.formula(plotFormula), data = obj, main = title, xlab = xLabs, 
						ylab = yLabs, auto.key = auto.key, groups = eval(parse(text = gVar)),
						par.settings = par.settings, strip = getStripFun(), 
						outer = TRUE, layout = layout, 
						panel = panel.nmDotPlot, scales = scales, ...) 
	
	if (xAxisPlotStyle == "cat") {
		dPlot <- bwplot(as.formula(plotFormula), data = obj, main = title, xlab = xLabs, 
				ylab = yLabs, auto.key = auto.key, groups = eval(parse(text = gVar)),
				par.settings = par.settings, strip = getStripFun(), 
				outer = TRUE, layout = layout, 
				panel = panel.nmBwPlot, scales = scales, ...) 
	}
				
			# numeric(2)
	multiTrellis(list(dPlot), gridDims = c(1,1), maxPanels = maxPanels)
	
}

#' creates a grouping variable which splits the factor variable onto various panels.
#' 
#' each panel than indicated, but sometimes allows considerably less.
#'
#' @param var A vector with the values of the actual column 
#' @param varName Name of variable
#' @param maxPerPanel Maximum 
#' @title Factor-splitting trellis variable
#' @return A factor variable such that using it as trellis variable will split the y-axis variable across multiple
#' panels (with no more per panel equal to maxPerPanel
#' @author Mango Solutions
#' @noRd 

.factorTrellis <- function(var, maxPerPanel)
{
	varLevels <- unique(var)
	parallelVec <- seq_along(varLevels)
	names(parallelVec) <- as.character(varLevels)
	
	numPanels <- floor(length(varLevels) / maxPerPanel) + ifelse(length(varLevels) %% maxPerPanel > 0,  1, 0) 
	
	# replace the above line with the following ?:
	groupingVar <- rep(1:numPanels, each = maxPerPanel)
	groupingVar <- groupingVar[1:length(var)]
	
	groupingVar[ parallelVec[as.character(var)] ]
	
}

panel.nmDotPlot <- function(x, y, ... )
{
	panel.dotplot(x = x, y = y, ...)
}

panel.nmBwPlot <- function(x, y, ... )
{
	panel.bwplot(x = x, y = y, ...)
}


setGeneric("nmDotPlot")
setMethod("nmDotPlot", signature(obj = "data.frame"), nmDotPlot.data.frame)
setMethod("nmDotPlot", signature(obj = "NMProblem"), nmDotPlot.NMProblem)
setMethod("nmDotPlot", signature(obj = "NMRun"), nmDotPlot.NMRun)

