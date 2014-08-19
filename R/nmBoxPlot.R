
#' Creates a boxplot of continuous variables against factor variables
#' @name nmBoxPlot
#' @title NONMEM box plot
#' @param obj An object of class \code{NMRun}, \code{NMProblem}, or \code{data.frame}. The object from which data will be plotted.
#' @param contVar Character vector or comma-seperated list of continuous variables from which boxplots are created
#' @param factVar Factor variable (single string)
#' @param bVars Trellis variables, specified as characters (or \code{NULL}, which is the default).
#' @param iVar ubject identifier variable.
#' @param titles Main title. 
#' @param xLabs X-axis label.
#' @param yLabs Y-axis label.
#' @param xRotAngle Angle by which to rotate the x-axis tick marks
#' @param maxPanels Maximum number of panels that should appear on each page of a graph.
#' @param maxTLevels If a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned.
#'                      If a character vector or a vector of length greater than one, the explicit breakpoints.
#' @param yAxisScaleRelations Axis scale relations when multiple y-variables exist. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param subProblems The sub problem of a run with simulations. 
#' @param overlaid logical flag. Should multiple factor variables be plotted on a single plot, or should 
#' multiple subplots with different factors be generated? 
#' @param problemNum The problem number, required for a \code{NMRun} object.
#' @param contVarOnX If \code{TRUE}, will flip axes so that the continuous variable is on the x-axis.
#' @param layout Numeric vector giving the number of columns, rows and pages in a multipanel display.
#' @param factBin Single numeric. Factor variable will be binned to this many levels if it has more levels than this value.
#' Binning done according to "counts".
#' @param balancedContAxis Single logical. If TRUE, axis with continuous variable will extend equally in both directions 
#' @param medianLines Single logical. If TRUE, will plot median lines inside boxes instead of points (pch = 19)
#' @param modifyOnResidual Single logical. If TRUE, will force balanced continuous variable axis and a horizontal line 
#' at y = 0 IF there is only one continuous variable AND it is contained in residVars.
#' @param hLines Numeric vector. If not empty (NULL or zero length vector), horizontal lines will be added at y = (all values)
#' @param residVars A vector of character names to count as residual variables (for use with \code{modifyOnResidual})
#' @param xAxisScaleRelations Axis scale relations when multiple x-variables exist. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param ... Additional parameters to \code{bwplot}. 
#' @return Multitrellis class object containing the plot.
#' @author Mango Solutions
#' @examples
#' \dontrun{ 
#' Theoph.df <- as.data.frame(Theoph)
#' nmBoxPlot(Theoph.df, contVar = "conc", factVar = "Time", factBin = 6)
#' }
#' @keywords hplot
#' @exportMethod nmBoxPlot

# TODO: contVarOnX does not work with overLaid = TRUE

nmBoxPlot <- function(obj,contVar, factVar, bVars = NULL, iVar = "ID", titles = "", xLabs = NULL, 
		xRotAngle = 0, yLabs = NULL, overlaid = FALSE, contVarOnX = FALSE, layout = NULL, maxPanels = NULL, 
		maxTLevels = Inf, yAxisScaleRelations = c("same", "free", "sliced"), factBin = Inf, problemNum = 1,
		subProblems = 1, hLines = NULL, balancedContAxis = FALSE, 
		medianLines = TRUE, modifyOnResidual = TRUE, residVars = c("WRES", "IWRE", "IWRES"), 
        xAxisScaleRelations = c("same", "free", "sliced"), ...)
{
	RNMGraphicsStop("Not implemented for this class!")
}

nmBoxPlot.NMRun <- function(obj, contVar, factVar, bVars = NULL, iVar = "ID", titles = "", xLabs = NULL, 
		xRotAngle = 0,
		yLabs = NULL, overlaid = FALSE, contVarOnX = FALSE, layout = NULL, maxPanels = NULL, 
		maxTLevels = Inf, 	yAxisScaleRelations = c("same", "free", "sliced"), factBin = Inf, problemNum = 1,
		subProblems = 1, hLines = NULL, balancedContAxis = FALSE, 
		medianLines = TRUE, modifyOnResidual = TRUE, residVars =  c("WRES", "IWRE", "IWRES"),
        xAxisScaleRelations = c("same", "free", "sliced"),
        ...)
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmBoxPlot, x[-1])
}


setGeneric("nmBoxPlot")	

nmBoxPlot.NMProblem <- function(obj, contVar, factVar, bVars = NULL,iVar = "ID", titles = "", 
		xLabs = NULL, xRotAngle = 0, 
		yLabs = NULL, overlaid = FALSE, contVarOnX = FALSE,layout = NULL, maxPanels = NULL, 
		maxTLevels = Inf, 	yAxisScaleRelations = c("same", "free", "sliced"),factBin = Inf, problemNum = 1, 
		subProblems = 1,  hLines = NULL ,balancedContAxis = FALSE,  
		medianLines = TRUE, modifyOnResidual = TRUE, residVars =  c("WRES", "IWRE", "IWRES"),
        xAxisScaleRelations = c("same", "free", "sliced"), ...)
{
	dat <- nmData(obj, subProblemNum = subProblems)
	graphSubset(dat) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dat
	
	do.call(nmBoxPlot, x[-1])
	
}

nmBoxPlot.data.frame <- function(obj, contVar, factVar, bVars = NULL, iVar = "ID", titles = "", 
		xLabs = NULL, xRotAngle = 0, 
		yLabs = NULL, overlaid = FALSE, contVarOnX = FALSE, layout = NULL, maxPanels = NULL,
		maxTLevels = Inf, yAxisScaleRelations = c("same", "free", "sliced"), factBin = Inf, problemNum = 1,
		subProblems = 1,  hLines = NULL, balancedContAxis = FALSE, 
		medianLines = TRUE, modifyOnResidual = TRUE, residVars =  c("WRES", "IWRE", "IWRES"), 
        xAxisScaleRelations = c("same", "free", "sliced"),
        ...)
{
	# max length of a NONMEM variable/column
	
	NONMEMVARLENGTH <- 4
	
	# turn variables into vectors instead of comma seperated strings
	
	## include removeEmpty to prevent errors around empty strings
	## add some error handling around contVar if no variable is selected
	contVars <- CSLtoVector(contVar,removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(contVars) > 0, "At least one variable must be provided to contVar to create this plot.\n")
	
	## agott (14/08/13)
	## include removeEmpty option to prevent empty string errors and multiple 
	## variables passed to factVar
	factVar <- CSLtoVector(factVar, removeEmpty = TRUE)
	
    RNMGraphicsStopifnot(length(factVar) == 1, "Currently not allowing more than one factor variable")
	
	obj <- applyGraphSubset(obj)
	
	# bin the x variable if necessary
	
    if(length(unique(obj[[factVar]])) > factBin)
    {
		obj <- addDerivedCategorical(obj, factVar, paste(factVar, "BINNED", sep = "."), 
				breaks = factBin, binType = "counts")
	
		factVar <- paste(factVar, "BINNED", sep = ".")
	}
	
	contVarsCollapsed <- paste(contVars, collapse = "+")
	if(!contVarOnX)
    {	
        plotFormula <- paste(contVarsCollapsed, factVar, sep = "~")
    }    
	else 
    {
		plotFormula <-  paste( factVar, contVarsCollapsed, sep = "~")        
    }
			
	if(!is.null(bVars))
	{
		bVars <-CSLtoVector(bVars)
		temp <- processTrellis(obj, bVars, maxLevels = maxTLevels, exemptColumns = iVar)
		obj <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		
	    plotFormula <- paste(plotFormula, paste(bVars, collapse = "+"), sep = "|" )
    }
	
    obj <- coerceToFactors(obj, factVar)
    	
	
    if(is.null(xLabs))
    {
		if(contVarOnX)
        {
            xLabs <- contVarsCollapsed
        }
        
        else
        {
            xLabs <- factVar
        }            
    }
	
	if(is.null(yLabs))
    {
        if(contVarOnX)
        {
            yLabs <- factVar
        }
        
        else
        {
            yLabs <- contVarsCollapsed
        }      
    }
	
	if(length(maxPanels) > 0) layout <- NULL	
	# ensure that maxPanels is numeric, even if empty	
	else maxPanels <- numeric(0)
	
    graphParams <- getAllGraphParams()
	
	# if modifyOnResidual is TRUE, then we force a balanced continous axis and a horizontal line at y = 0.  Note that
	# This will NOT work for contVarOnX = TRUE OR if there are multiple continuous variables

	if(modifyOnResidual && (length(contVar) == 1) && !contVarOnX &&	contVar %in% residVars)
	{
		balancedContAxis = TRUE
		hLines <- c(hLines, 0)
	}
	
	
	# TODO: fix this.  For now, it is disabled
	
	if(overlaid && length(contVar) > 1 && all(sapply(obj[, contVar], class) == "numeric"))
	{
		stackedData <- stack(obj[, contVar])
		lev <- levels(obj[[factVar]])
		df <- data.frame(y = stackedData$values, x = rep(obj[[factVar]], length(contVar)), which = gl(length(contVar), length(obj[[factVar]])))	
		bwplot(y ~ x:which, data = df, horizontal = FALSE, ylab = NULL, xlab = factVar, groups = which,
				panel = panel.superpose, panel.groups = panel.bwplot, pch = "|", 
				key = simpleKey(contVar, points = FALSE, rectangles = TRUE, space = "right", cex = 0.7),
				scales = list(x = list(labels = rep(lev, each = length(contVar)))))
	}
	else
	{
		stripfn = getStripFun()
		scales <- list(x = list(rot = xRotAngle), y = list())
		
        scales$y$relation <- match.arg(yAxisScaleRelations)
        scales$x$relation <- match.arg(xAxisScaleRelations)
	
		# if median lines were requested, use them, else median points
	
		if(medianLines) pch <- "|" else pch <- 19	
		
        boxPlot <- with(graphParams,
				# TODO: passing options this way is getting unwieldy
				bwplot(as.formula(plotFormula), data = obj, main = titles, 
						xlab = xLabs, horizontal = contVarOnX, scales = scales, 
					 ylab = yLabs,  hLines = hLines,  panel = panel.nmBoxPlot,
					 prepanel = prepanel.nmBoxPlot, balanced = balancedContAxis, pch = pch,
					 par.settings = list(plot.symbol = plot.symbol, add.line = refline, 
				 		par.xlab.text = axis.text, 
						par.ylab.text = axis.text,par.main.text = title.text,
						box.rectangle = boxplot[c("alpha","col","fill","lty","lwd")],
						box.umbrella = list(col = boxplot$umb.col, lty = boxplot$umb.lty, 
											lwd = boxplot$umb.lwd),
									strip.background = strip.bg), 
								strip = stripfn, outer = TRUE, layout = layout, ...)
				) # end with(graphParams, 
		
		multiTrellis(list(boxPlot), c(1,1), maxPanels = maxPanels)
	} # end else
}

#' @description panel functions used for different objects.
#' @details panel.nmBoxPlot is the nmBoxPlot custom panel function. Mainly used to add reference lines.
#' @name panel functions
#' @aliases panel.nmBoxPlot panel.nmHistogram panel.overlaidScatter prepanel.nmBoxPlot
#' @param x passed straight to panel.bwplot
#' @param y passed straight to panel.bwplot
#' @param hLines Vector of horizontal reference lines
#' @param ... 
#' @title panel functions
#' @return NULL
#' @author Mango Solutions

panel.nmBoxPlot <- function(x, y, hLines = NULL, ...)
{
	
	panel.bwplot(x = x, y = y, ...)
	# 
	if(length(hLines) > 0 & is.numeric(hLines))
		panel.abline(h = hLines)
}

#' @details prepanel.nmBoxPlot is the prepanel function for nmBoxPlot. It is needed because in certain cases we want the axis holding the continous 
#' to be "balanced"
#' @name panel functions
#' @aliases panel.nmBoxPlot panel.nmHistogram panel.overlaidScatter prepanel.nmBoxPlot
#' @param x passed straight to prepanel.default.bwplot
#' @param y passed straight to prepanel.default.bwplot
#' @param horizontal logical flag.  Passed straight to prepanel.default.bwplot
#' @param balanced If TRUE, will "balance" the axis with the continuous variable on it
#' @param ... Additional arguments passed to prepanel.default.bwplot

prepanel.nmBoxPlot <-function(x, y, horizontal, balanced = FALSE, ...)
{
	prePan <- prepanel.default.bwplot(x,y, horizontal = horizontal, ...)
	if(balanced)
	{
		# force the axis limits to be (-a, a)
		if(horizontal)
		{
			limMax <- max(abs(prePan$xlim))
			prePan$xlim <- c(-limMax, limMax)
		}
		else
		{
			limMax <- max(abs(prePan$ylim))
			prePan$ylim <- c(-limMax, limMax)
		}
	}
	prePan
}

setMethod("nmBoxPlot", signature(obj = "data.frame"), nmBoxPlot.data.frame)
setMethod("nmBoxPlot", signature(obj = "NMRun"), nmBoxPlot.NMRun)
setMethod("nmBoxPlot", signature(obj = "NMProblem"), nmBoxPlot.NMProblem)