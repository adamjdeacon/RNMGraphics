# TODO: Add comment
# $Rev$
# $LastChangedDate$
# Author: fgochez
###############################################################################



#' Create a custom dotplot of one or more continuous variables against a categorical variable
#' @name NONMEM dot plot
#' @title NONMEM dot plot
#' @param obj "NONMEM" object
#' @param factVar Categorical variable name
#' @param contVar One or more continuous variables (comma seperated list or vector)
#' @param bVars One or more trellis/by variables
#' @param iVar Subject identifier variable
#' @param gVar Grouping variable
#' @param title Plot main title
#' @param xLabs X-axis label
#' @param yLabs Y-axis label
#' @param layout Trellis layout - is overwritteen by maxPanels
#' @param maxPanels Max panels per page
#' @param addLegend Add a legend?
#' @param maxTLevels Bin trellis variable if it has more than this many levels
#' @param ... 
#' @return 
#' @author fgochez
#' @keywords

nmDotPlot <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf, maxFactPerPanel = Inf, problemNum = 1, subProblems = 1,
		...)   
{
	RNMGraphicsStop("Not implemented for this class yet \n")	
}

nmDotPlot.NMRun <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf, maxFactPerPanel = Inf, problemNum = 1, subProblems = 1,
		...)   
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmDotPlot, x[-1])
}

# TODO: unit test maxFactPerPanel
nmDotPlot.NMProblem  <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf,  maxFactPerPanel = Inf,  
		problemNum = 1, subProblems = 1,...)
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
					problemNum = 1, subProblems = 1,...)   
{


	contVar <- CSLtoVector(contVar)
	factVar <- CSLtoVector(factVar)
	
	varCombos <- varComboMatrix(factVar, contVar, collapseY = TRUE, collapseX =FALSE)
	numCombos <- nrow(varCombos)
	
	plotFormulas <- apply(varCombos, 1, paste, collapse = "~")
	obj <- applyGraphSubset(obj, graphSubset(obj))
	if(!is.null(bVars)) bVars <- CSLtoVector(bVars)
	
	scales <- list(x = list(), y = list())
	# force factVar to be a factor
	obj <- coerceToFactors(obj, factVar)
	
	# group the factor variable if there are too many levels, leading to "crammed" axis labels
	if(length(unique(obj[[factVar]])) > maxFactPerPanel)
	{
		
		# sort by factor first, if possible
		obj <- obj[order(as.numeric(obj[[factVar]])),]
		
		x <- .factorTrellis(obj[[factVar]], factVar, maxFactPerPanel)
		obj[[paste(factVar, "GRP", sep = ".")]] <- x
		bVars <- c(bVars, paste(factVar, "GRP", sep = "."))
		# need to free y axis relations to take advantage of this
		
		scales$y$relation <- "free"
		
	}
	
	if(!is.null(bVars))
	{
		
		temp <- processTrellis(obj, bVars, maxLevels = maxTLevels, exempt = iVar)
		obj <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- sapply(1:numCombos, function(i) paste(plotFormulas[i], paste(bVars, collapse = "+"), sep = "|"))
	}
	
	
	plotList <- vector(mode = "list", length = numCombos)
	
	repeatVars(c("title"), list(title), numCombos)
	
	if(!is.null(xLabs))
		xLabs <- rep(CSLtoVector(xLabs), length.out = numCombos)
	else
		xLabs <- as.character(varCombos[,2])
	
	if(!is.null(yLabs))
		yLabs <- rep(CSLtoVector(yLabs), length.out = numCombos)
	else
		yLabs <- as.character(varCombos[,1])
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
	# TODO: at the moment, title = gVar rather than its label.  This is because 
	# the full description often causes overflows (e.g. when gVar = EVID)
	auto.key <- if(addLegend) list(title = getVarLabel(gVar), cex=.7, rows=10,space="right") else NULL

	for(i in seq_along(plotFormulas))
	{
		
		plotList[[i]] <- 
				dotplot(as.formula(plotFormulas[i]), data = obj, main = title, xlab = xLabs[i], 
						ylab = yLabs[i], auto.key = auto.key, groups = eval(parse(text = gVar)),
						par.settings = par.settings, strip = getStripFun(), 
						outer = TRUE, layout = layout, 
						panel = panel.nmDotPlot, scales = scales, ...)
	} 
	gridDims <- stdGridDims(numCombos,3 )
			# numeric(2)
	multiTrellis(plotList, gridDims, maxPanels = maxPanels)
	
}

# creates a grouping variable which splits the x-axis variable onto various panels.

.factorTrellis <- function(var,varName, maxPerPanel)
{
	varLevels <- unique(var)
	parallelVec <- seq_along(varLevels)
	names(parallelVec) <- as.character(varLevels)
	numPanels <- floor(length(varLevels) / maxPerPanel) + ifelse(length(var) %% maxPerPanel > 0,  1, 0)
	groupingVar <- rep(1:numPanels, length.out = length(varLevels)); groupingVar <- sort(groupingVar) 
	
	groupingVar[ parallelVec[as.character(var)] ]
	
}

panel.nmDotPlot <- function(x, y, ... )
{
	panel.dotplot(x = x, y = y, ...)
}

setGeneric("nmDotPlot")
setMethod("nmDotPlot", signature(obj = "data.frame"), nmDotPlot.data.frame)
setMethod("nmDotPlot", signature(obj = "NMProblem"), nmDotPlot.NMProblem)
setMethod("nmDotPlot", signature(obj = "NMRun"), nmDotPlot.NMRun)

