# $Rev$
# $LastChangedDate$

#' Creates a boxplot of continuous variables against factor variables
#' @name nmBoxPlot
#' @title NONMEM box plot
#' @param obj NMRun, NMProblem or data.frame object from which to plot
#' @param contVar continuous variables
#' @param factVar factor variables
#' @param bVars "by" variables
#' @param titles Plot titles
#' @param xLabs x-axis labels
#' @param yLabs y-axis labels
#' @param overlaid logical flag. Should multiple factor variables be plotted on a single plot, or should 
#' multiple subplots with different factors be generated?
#' @param problemNum Number of the problem to use if obj is of class NMRun
#' @param ... additional parameters passed to bwplot
#' @return A multiTrellis object
#' @author fgochez
#' @keywords hplot

nmBoxPlot <- function(obj,contVar, factVar, bVars = NULL, titles = "", xLabs = NULL, 
		yLabs = NULL, overlaid = FALSE, problemNum = 1, ...)
{
	RNMGraphicsStop("Not implemented for this class!")
}
setGeneric("nmBoxPlot")	

nmBoxPlot.NMBasicModel <- function(obj, contVar, factVar, bVars = NULL, titles = "", xLabs = NULL, 
		yLabs = NULL, overlaid = FALSE, problemNum = 1, ...)
{
	dat <- nmData(obj)
	nmBoxPlot(dat, contVar, factVar, bVars = NULL, titles = "", xLabs = NULL, 
			yLabs = NULL, problemNum = 1, ...)
	
}

nmBoxPlot.data.frame <- function(obj, contVar, factVar, bVars = NULL, titles = "", xLabs = NULL, 
		yLabs = NULL, overlaid = FALSE, problemNum = 1, ...)
{
	contVar <- CSLtoVector(contVar)
	factVar <- CSLtoVector(factVar)
	
	varCombos <-  expand.grid(contVar, factVar)
	numCombos <- nrow(varCombos)
	
	plotFormulas <- apply(varCombos, 1, paste, collapse = "~")
	
	if(!is.null(bVars))
	{
		bVars <-CSLtoVector(bVars)
		obj <- coerceToFactors(obj, bVars)
		plotFormulas <- sapply(1:numCombos, function(i) paste(plotFormulas[i], paste(bVars, collapse = "+"), sep = "|"))
	}
	obj <- coerceToFactors(obj, factVar)
	plotList <- vector(mode = "list", length = numCombos)
	repeatVars(c("titles"), list(titles), numCombos)
	if(!is.null(xLabs))
		xLabs <- rep(CSLtoVector(xLabs), length.out = numCombos)
	else
		xLabs <- as.character(varCombos[,2])
	
	if(!is.null(yLabs))
		yLabs <- rep(CSLtoVector(yLabs), length.out = numCombos)
	else
		yLabs <- as.character(varCombos[,1])
	
	graphParams <- getAllGraphParams()
	
	if(overlaid && length(contVar) > 1 && all(sapply(obj[, contVar], class) == "numeric"))
	{
		stackedData <- stack(obj[, contVar])
		lev <- levels(obj[[factVar]])
		df <- data.frame(y = stackedData$values, x = rep(obj[[factVar]], length(contVar)), which = gl(length(contVar), length(obj[[factVar]])))	
		bwplot(y ~ x:which, data = df, horizontal = FALSE, ylab = NULL, xlab = factVar, groups = which,
				panel = panel.superpose, panel.groups = panel.bwplot, pch = "|", key = simpleKey(contVar, points = FALSE, rectangles = TRUE),
				scales = list(x = list(labels = rep(lev, each = length(contVar)))))
	}
	else
	{
		stripfn = getStripFun()
		for(i in seq_along(plotFormulas))
			plotList[[i]] <- with(graphParams,
				# TODO: passing options this way is getting unwieldy
				 			bwplot(as.formula(plotFormulas[[i]]), data = obj, main = titles[[i]], xlab = xLabs[i], 
									ylab = yLabs[i], 
									par.settings = list(plot.symbol = plot.symbol,
											par.xlab.text = axis.text, 
									par.ylab.text = axis.text,par.main.text = title.text,
									box.rectangle = boxplot[c("alpha","col","fill","lty","lwd")],
									box.umbrella = list(col = boxplot$umb.col, lty = boxplot$umb.lty, 
											lwd = boxplot$umb.lwd),
									strip.background = strip.bg), 
								strip = stripfn, ...)
				) # end with(graphParams, 
	
		gridDims <- numeric(2)
		gridDims[2] <- min(3, length(plotList))
		gridDims[1] <- ceiling(numCombos / gridDims[2])
	
		multiTrellis(plotList, gridDims)
	} # end else
}

setMethod("nmBoxPlot", signature(obj = "data.frame"), nmBoxPlot.data.frame)