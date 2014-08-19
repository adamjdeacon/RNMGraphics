
#' Generates a categorical barchart of a set of categorical variables against another one.
#' @name nmBarChart
#' @title NONMEM categorical barchart
#' @param obj NMProblem or data.frame object
#' @param xVars Character vector specifying x-axis variable.  Only one may be specified at the moment.  
#' @param yVars Character vector specifying y-axis variable. Only one may be specified at the moment. 
#' @param bVars Trellis variables, specified as characters (or \code{NULL}, which is the default).
#' @param xLab x-axis label
#' @param yLab y-axis label
#' @param titles Main plot title
#' @param xRotAngle Angle by which to rotate x-angle tick labels.
#' @param xBin X-axis binning: Either a single value for range binning or explicit cut points.
#' @param addLegend  Logical. Should we add a legend to the plot? 
#' @param problemNum The problem required for a \code{NMRun} object. 
#' @param subProblems The sub problem of a run with simulations.
#' @param yAxisScaleRelations Y-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param xAxisScaleRelations X-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param ... additional parameters passed to the barchart function
#' @return Multitrellis class object
#' @examples
#' \dontrun{ 
#' nmBarChart(mtcars, xVars = "cyl", yVars = "gear", bVars = "vs")
#' }
#' @author Mango Solutions
#' @keywords hplot
#' @exportMethod nmBarChart

nmBarChart <- function(obj, xVars, yVars, bVars = NULL, xLab = NULL, xRotAngle = 0, yLab = NULL, titles = NULL, addLegend = TRUE 
					   ,xBin = Inf , problemNum = 1, subProblems = 1, 
                       yAxisScaleRelations = c("same", "free", "sliced"),
                       xAxisScaleRelations = c("same", "free", "sliced"),
                       ...)
{
	RNMGraphicsStop("Not implemented for this class!", call = match.call())
}

setGeneric("nmBarChart")

nmBarChart.NMRun <- function(obj, xVars, yVars, bVars = NULL, xLab = NULL, xRotAngle = 0, yLab = NULL, titles = NULL, addLegend = TRUE 
		,xBin = Inf , problemNum = 1, subProblems = 1, 
        yAxisScaleRelations = c("same", "free", "sliced"),
        xAxisScaleRelations = c("same", "free", "sliced"),
        ...)
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmBarChart, x[-1])
}

nmBarChart.NMProblem <- function(obj, xVars, yVars, bVars = NULL, xLab = NULL, xRotAngle = 0 , yLab = NULL, titles = NULL, 
		addLegend = TRUE, xBin = Inf, problemNum = 1, subProblems = 1, 
        yAxisScaleRelations = c("same", "free", "sliced"),
        xAxisScaleRelations = c("same", "free", "sliced"),
        ...)
{
	callList <- as.list(match.call())
	callList$obj <- nmData(obj, subProblemNum = subProblems)
	graphSubset(callList$obj) <- graphSubset(obj)
	do.call(nmBarChart, callList[-1])
}

# TODO: test xBin

nmBarChart.data.frame <- function(obj, xVars, yVars, bVars = NULL, xLab = NULL, xRotAngle = 0, yLab = NULL, titles = "", 
		addLegend = TRUE, xBin = Inf, problemNum = 1, subProblems = 1,
        yAxisScaleRelations = c("same", "free", "sliced"),
        xAxisScaleRelations = c("same", "free", "sliced"),
        ...)
{
	xVars <- CSLtoVector(xVars, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(xVars) == 1, "Currently not accepting more than one X variable\n")
	yVars <- CSLtoVector(yVars, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(yVars) == 1, "Currently not accepting more than one Y variable\n")
	
	if(!(xVars %in% names(obj))){ 
		obj$dummy <- ""
		xVars <- "dummy"
	}
	
    obj <- applyGraphSubset(obj)
	
    if (!is.null(bVars))
    {
        bVars <- CSLtoVector(bVars)
    }
    
	## bin the x variable if necessary.  
	# If xBin is a character, it will be something like "4" or "1, 2, 3" (explicit break-points), so this needs
    # to be turned into a numeric vector
    
    if (is.character(xBin)) 
    {
		nowWarn <- options()$warn
		options(warn = 2)
		xBin <- try(as.numeric(CSLtoVector(xBin)), silent = TRUE)
		if (class(xBin) == "try-error") RNMGraphicsStop("Could not parse maximum levels input")
		options(warn = nowWarn)
	}
    
    # xBin is a single numeric, so break into that number of bins
    
	if(length(xBin) == 1 && length(unique(obj[[xVars]])) > xBin) 
    {
		obj <- addDerivedCategorical(obj, xVars, paste(xVars, "BINNED", sep = "."), breaks = xBin, binType = "counts")
		xVars <- paste(xVars, "BINNED", sep = ".")
	}
    
    # xBin is a set of explicit breaks
    
	if (length(xBin) > 1) 
    {
		obj <- addDerivedCategorical(obj, xVars, paste(xVars, "BINNED", sep = "."), breaks = xBin, binType = "explicitcuts")
		xVars <- paste(xVars, "BINNED", sep = ".")
	}
	
	# take all combinations of x variables against y variables

	if(is.null(xLab))
    {
		xLab <- xVars
    }
	
    if(is.null(yLab)) 
    {
		yLab <- yVars
    }
    
    graphParams <- getAllGraphParams()
    
    # if there are trellis variables, a fair amount of manipulation must be performed as the barchart
    # function does not make this easy
    
    if (length(bVars) > 0) 
    {
        tab <- do.call("table", obj[c(xVars, yVars, bVars)])
        tabDf <- as.data.frame.table(tab)
        splitList <- split(tabDf, tabDf[[yVars]])
        coreData <- splitList[[1]][c(xVars, bVars)]
        addData <- data.frame(lapply(splitList, "[[", "Freq"))
        addData <- data.frame(t(apply(addData, 1, function(x) {
                                    sumX <- sum(x); if (sumX == 0) return(x) else return(x / sumX)				
                                })))
        names(addData) <- paste(".Y", 1:ncol(addData), sep="")
        plotData <- cbind(coreData, addData)
        plotForm <- paste(paste(names(addData), collapse = "+"), "~", xVars)
        plotForm <- paste(plotForm, paste(bVars, collapse= "*"), sep="|")
    }
    
    else 
    {
        tab <- table(obj[[xVars]], obj[[yVars]])
        tab <- sweep(tab, 1, rowSums(tab), "/")
        tab <- as.data.frame(unclass(tab))
        names(tab) <- paste(".Y", 1:ncol(tab), sep="")
        plotData <- data.frame(X = sort(unique(obj[[xVars]])), tab)
        plotForm <- paste(paste(names(tab), collapse = "+"), "~ X")
    }
    
    if(addLegend) 
    {
        key <- list(text = as.character(sort(unique(obj[[yVars]]))),
                title = getVarLabel(yVars), 
                points = FALSE, rectangles = TRUE, space = "right", cex = 0.8, 
                columns = .legendColumns(length(unique(obj[[yVars]] )) ))
    }
    
    else key <- NULL
    
    scales <- list( x = list(rot = xRotAngle, relation = match.arg(xAxisScaleRelations)), y = list(relation = match.arg(yAxisScaleRelations) ))
    stripFun <- getStripFun()
    bChart <- barchart(x = as.formula(plotForm), data = plotData, 
            xlab = xLab, ylab = yLab, horizontal = FALSE, stack = TRUE, 
            scales = scales, par.settings = mapTopar.settings(graphParams), 
            auto.key = key, main = titles, strip = stripFun, ...)
    
    
    result <- multiTrellis(list(bChart), c(1,1))
    
    result
    
}

.legendColumns <- function(numLevels)
{
	MAXROWS <- 25
	
	floor(numLevels / MAXROWS) + ifelse(numLevels %% MAXROWS > 0, 1, 0)
}

setMethod("nmBarChart", signature(obj = "data.frame"), nmBarChart.data.frame)
setMethod("nmBarChart", signature(obj = "NMProblem"), nmBarChart.NMProblem)
setMethod("nmBarChart", signature(obj = "NMRun"), nmBarChart.NMRun)