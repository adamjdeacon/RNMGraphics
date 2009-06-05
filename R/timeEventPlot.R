# TODO: Add comment
# $Rev$
# $LastChangedDate$
# Author: fgochez
###############################################################################
 
# TODO element comments, design docs

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
		evtVar = "EVID", iVar = "ID",title = "Time to event", xLab = "TIME", yLab = "ID", addLegend = TRUE, ...) 
{
	RNMGraphicsStop("Not implemented for this class\n")
	
}

timeEventDPlot.data.frame <- function(obj, tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",title = "Time to event", xLab = "TIME", yLab = "ID", addLegend = TRUE, ...) 
{
		
	nmDotPlot(obj, factVar = iVar, contVar = tVar, gVar = evtVar, xLab = xLab, yLab = yLab, addLegend = addLegend, ...)
}

setGeneric("timeEventDPlot")
setMethod("timeEventDPlot", signature(obj = "data.frame"), timeEventDPlot.data.frame)


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
		title = NULL, xLab = NULL, yLab = NULL, addLegend = TRUE, ...) 
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

timeEventSPlot.data.frame <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, addLegend = TRUE, ...) 
{
	# TODO: UNITTEST
	if(is.null(xLab)) xLab <- "Time"
	if(is.null(yLab)) yLab <- "Concentration"
	if(is.null(title)) title <- "Concentration vs Time"
	if(is.null(subjectNum)) subjectNum <- unique(obj[[iVar]])
	
	form <- paste(concVar, tVar, sep = "~")
	form <- paste(form, iVar, sep = "|")
	
	# subset the object
	obj <- obj[obj[[iVar]] %in% subjectNum,]
	obj[[iVar]] <- as.factor(obj[[iVar]])
	doses <- obj[c(doseVar, evtVar, tVar)]
	varMapping = c("tVar" = tVar, "doseVar" = doseVar, "evtVar" = evtVar)
	graphParams <- getAllGraphParams()
	plotKey <- list(text = list(c("Concentration", "Dose"), space = "right"),
			lines = list(lty = 1:2, col = c(graphParams$plot.line$col, "black")))
#	plotKey <- list(text = c("Concentration", "Dose"), lines = TRUE,
		#points = FALSE, lty = 1:2, col = c("darkblue", "black"))
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
			key = plotKey ,...))
	multiTrellis(list(plt))
	# plt
}

setGeneric("timeEventSPlot")

setMethod("timeEventSPlot", signature(obj = "data.frame"), timeEventSPlot.data.frame)