
# $Rev$
# $LastChangedDate$
# Author: fgochez
###############################################################################
 
# TODO element comments, design docs
# TODO: legend in timeEventSPlot does not capture the lty of the dose line

#' 
#' @name
#' @title
#' @param obj 
#' @param tVar 
#' @param doseVar 
#' @param evtVar 
#' @param iVar 
#' @param title 
#' @param xLab 
#' @param yLab 
#' @param addLegend 
#' @param ... 
#' @return 
#' @author fgochez
#' @keywords
timeEventDPlot<- function(obj, tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",title = "Time to event", xLab = "TIME", yLab = "ID",
		addLegend = TRUE, problemNum = 1, subProblems = 1, ...) 
{
	RNMGraphicsStop("Not implemented for this class\n")
	
}

timeEventDPlot.NMRun <- function(obj, tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",title = "Time to event", xLab = "TIME",
		yLab = "ID", addLegend = TRUE, problemNum = 1, subProblems = 1, ...) 
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(timeEventDPlot, x[-1])
	
	
}

timeEventDPlot.NMProblem <- function(obj, tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",title = "Time to event", xLab = "TIME",
		yLab = "ID", addLegend = TRUE, problemNum = 1, subProblems = 1, ...) 
{

	dataSet <- nmData(obj, subProblemNum = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(timeEventDPlot, x[-1])
	
	
}


timeEventDPlot.data.frame <- function(obj, tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",title = "Time to event", xLab = "TIME", yLab = "ID", 
		addLegend = TRUE, problemNum = 1, subProblems = 1, ...) 
{
		
	nmDotPlot(obj, factVar = iVar, contVar = tVar, gVar = evtVar, xLab = xLab, yLab = yLab, addLegend = addLegend, 
			title = title, problemNum = 1, subProblems = 1, ...)
}

setGeneric("timeEventDPlot")

setMethod("timeEventDPlot", signature(obj = "data.frame"), timeEventDPlot.data.frame)

setMethod("timeEventDPlot", signature(obj = "NMRun"), timeEventDPlot.NMRun)
setMethod("timeEventDPlot", signature(obj = "NMProblem"), timeEventDPlot.NMProblem)


#' 
#' @name
#' @title
#' @param obj 
#' @param concVar 
#' @param tVar 
#' @param doseVar 
#' @param evtVar 
#' @param iVar 
#' @param subjectNum 
#' @param title 
#' @param xLab 
#' @param yLab 
#' @param addLegend 
#' @param ... 
#' @return 
#' @author fgochez
#' @keywords

timeEventSPlot <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, addLegend = TRUE, 
		layout = NULL, maxPanels = NULL,
		problemNum = 1, subProblems = 1,
		...) 
{
	RNMGraphicsStop("Not implemented for this class\n")
}

panel.timeEventSPlot <- function(x, y, subscripts, doseInfo, colNames = c("tVar" = "TIME", 
				"doseVar" = "AMT", "evtVar" =  "EVID"),	...)
{
	
	panel.xyplot(x, y, subscripts = subscripts, ...)
	doseInfo <- doseInfo[subscripts,][doseInfo[[colNames["evtVar"]]] != 0,]
	panel.abline(v = doseInfo[,colNames["tVar"]], col = "black", lty = 2)
	
}

timeEventSPlot.NMRun <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, addLegend = TRUE, 
		layout = NULL, maxPanels = NULL,
		problemNum = 1, subProblems = 1,
		...) 
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(timeEventSPlot, x[-1])
	
}

timeEventSPlot.NMProblem <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, addLegend = TRUE, 
		layout = NULL, maxPanels = NULL,
		problemNum = 1, subProblems = 1,
		...) 
{
	dataSet <- nmData(obj, subProblemNum = subProblems)
	graphSubset(dataSet) <- graphSubset(obj)
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(timeEventSPlot, x[-1])
}


timeEventSPlot.data.frame <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, addLegend = TRUE,layout = NULL, maxPanels = NULL, 
		problemNum = 1, subProblems = 1, ...) 
{
	
	obj <- applyGraphSubset(obj)
	
	if(is.null(xLab)) xLab <- "Time"
	if(is.null(yLab)) yLab <- "Concentration"
	if(is.null(title)) title <- "Concentration vs Time"
	if(is.null(subjectNum)) subjectNum <- unique(obj[[iVar]])
	if(length(maxPanels) > 0) layout <- NULL
	# ensure that maxPanels is numeric, even if empty
	else maxPanels <- numeric(0)
	form <- paste(concVar, tVar, sep = "~")
	form <- paste(form, iVar, sep = "|")
	
	# subset the object
	obj <- obj[obj[[iVar]] %in% subjectNum,]
	obj[[iVar]] <- as.factor(obj[[iVar]])
	doses <- obj[c(doseVar, evtVar, tVar)]
	varMapping = c("tVar" = tVar, "doseVar" = doseVar, "evtVar" = evtVar)
	graphParams <- getAllGraphParams()
	plotKey <- list(text = list(c("Concentration", "Dose"), space = "right"),
			lines = list(lty = 1:2, col = c(graphParams$plot.line$col, "black")), cex = 0.7)
	plt <- with(graphParams, 
			xyplot(as.formula(form), type = "l", data = obj,
				par.settings = list(
					plot.symbol = plot.symbol, 
					
					par.xlab.text = axis.text, par.ylab.text = axis.text,
					par.main.text = title.text, plot.line = plot.line,
					add.line = refline, strip.background = strip.bg,  
					layout.widths = layout.widths, layout.heights = layout.heights), 
			strip = strip$stripfun, panel = panel.timeEventSPlot, doseInfo = doses, colNames = varMapping,
			xlab = xLab, ylab = yLab, main = title,
			key = plotKey , layout = layout, ...) )
	multiTrellis(list(plt), maxPanels = maxPanels)
	# plt
}

setGeneric("timeEventSPlot")

setMethod("timeEventSPlot", signature(obj = "data.frame"), timeEventSPlot.data.frame)
setMethod("timeEventSPlot", signature(obj = "NMRun"), timeEventSPlot.NMRun)
setMethod("timeEventSPlot", signature(obj = "NMProblem"), timeEventSPlot.NMProblem)