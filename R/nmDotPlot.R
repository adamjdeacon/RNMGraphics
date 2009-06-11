# TODO: Add comment
# $Rev$
# $LastChangedDate$
# Author: fgochez
###############################################################################


#' 
#' @name
#' @title
#' @param obj 
#' @param factVar 
#' @param contVar 
#' @param bVars 
#' @param gVar 
#' @param iVar 
#' @param title 
#' @param xLabs 
#' @param yLabs 
#' @param addLegend 
#' @param ... 
#' @return 
#' @author fgochez
#' @keywords
nmDotPlot <- function(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
		title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL, maxPanels = numeric(0),
		addLegend = TRUE, maxTLevels = Inf, ...)   
{
	RNMGraphicsStop("Not implemented for this class yet \n")	
}


# TODO: allow multiple factVars and contVars

nmDotPlot.data.frame <- function(obj, factVar, contVar, bVars = NULL,iVar = "ID", gVar = "NULL", 
					title = NULL, xLabs = NULL, yLabs = NULL,layout = NULL, maxPanels = numeric(0),	
					addLegend = TRUE, maxTLevels = Inf, ...)   
{

	# TODO : an excess of copy-paste is cropping up - try to find a way to reduce this
	contVar <- CSLtoVector(contVar)
	factVar <- CSLtoVector(factVar)
	
	varCombos <- varComboMatrix(factVar, contVar, collapseY = TRUE, collapseX =FALSE)
	numCombos <- nrow(varCombos)
	
	plotFormulas <- apply(varCombos, 1, paste, collapse = "~")
	
	if(!is.null(bVars))
	{
		bVars <-CSLtoVector(bVars)
		temp <- processTrellis(obj, bVars, maxLevels = maxTLevels, exempt = iVar)
		obj <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- sapply(1:numCombos, function(i) paste(plotFormulas[i], paste(bVars, collapse = "+"), sep = "|"))
	}
	
	obj <- coerceToFactors(obj, factVar)
	
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
	
	auto.key <- if(addLegend) list(title = gVar, cex=.7, rows=10,space="right") else NULL

	for(i in seq_along(plotFormulas))
	{
		# TODO: strip is broken, don't know why
		# TODO: strip background colour not being captured!
		plotList[[i]] <- 
				dotplot(as.formula(plotFormulas[i]), data = obj, main = title, xlab = xLabs[i], 
						ylab = yLabs[i], auto.key = auto.key, groups = eval(parse(text = gVar)),
						par.settings = par.settings, strip = getStripFun(), 
						outer = TRUE, layout = layout, ...)
	} 
	gridDims <- stdGridDims(numCombos,3 )
			# numeric(2)
	multiTrellis(plotList, gridDims, maxPanels = maxPanels)
	
}

setGeneric("nmDotPlot")
setMethod("nmDotPlot", signature(obj = "data.frame"), nmDotPlot.data.frame)
