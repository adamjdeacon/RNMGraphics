
#' Generates dot plots of dosing events against time for each individual in a NONMEM object.
#' @name timeEventDPlot
#' @title NONMEM Time to Event Dot Plot 
#' @param obj An object of class \code{NMRun}, \code{NMProblem}, or \code{data.frame}. The object from which data will be plotted.
#' @param tVar Time variable.
#' @param doseVar Dosing amount identifier variable. 
#' @param evtVar Event identifier variable.
#' @param iVar Subject identifier variable. 
#' @param title Main title
#' @param xLab x-axis label
#' @param yLab y-axis label
#' @param addLegend Should legends be added?
#' @param problemNum The problem required for a \code{NMRun} object. 
#' @param subProblems The sub problem of a run with simulations.
#' @param ... 
#' @return Multitrellis class object containing the plot.
#' @author Mango Solutions
#' @examples
#' \dontrun{ 
#' Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0)) ))
#' timeEventDPlot(Theoph2, tVar = "Time", concVar = "conc", 
#'   doseVar = "Dose", evtVar = "Evt", iVar = "Subject")  
#' }
#' @keywords hplot
#' @exportMethod timeEventDPlot

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
		
	nmDotPlot(obj, factVar = iVar, contVar = tVar, gVar = evtVar, xLabs = xLab, yLabs = yLab, addLegend = addLegend, 
			title = title, problemNum = 1, subProblems = 1, ...)
}

setGeneric("timeEventDPlot")

setMethod("timeEventDPlot", signature(obj = "data.frame"), timeEventDPlot.data.frame)

setMethod("timeEventDPlot", signature(obj = "NMRun"), timeEventDPlot.NMRun)
setMethod("timeEventDPlot", signature(obj = "NMProblem"), timeEventDPlot.NMProblem)


#' Generate scatter plot of concentration/DV against time for each individual in
#' a NONMEM object.  These will be line plots, with vertical reference lines at dosing times
#' @name timeEventSPlot
#' @title NONMEM Time to Event Scatter Plot
#' @param obj An object of class \code{NMRun}, \code{NMProblem}, or \code{data.frame}. The object from which data will be plotted. 
#' @param concVar Dependent variable.
#' @param tVar Time variable.
#' @param doseVar Dosing amount variable.
#' @param evtVar Event identifier variable. 
#' @param iVar Subject identifier variable.
#' @param subjectNum A vector of subject numbers. Otherwise, all subject numbers are plotted.
#' @param title Main title
#' @param xLab X-axis label
#' @param yLab Y-axis label
#' @param layout Numeric vector giving the number of columns, rows and pages in a multipanel display
#' @param maxPanels Maximum number of panels to display on each page 
#' @param problemNum The problem required for a \code{NMRun} object
#' @param subProblems The sub problem of a run with simulations
#' @param yAxisScaleRelations Y-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param xAxisScaleRelations X-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}.
#' @param expX Logical flag.  Exponentiate x-axis variable?
#' @param expY Logical flag.  Exponentiate the y-axis variables?
#' @param xRotAngle Single numeric.  Angle by which to rotate 
#' @param ... Additional parameters to xyplot
#' @return 	Multitrellis class object containing the plot.
#' @author Mango Solutions
#' @examples
#' \dontrun{ 
#' Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0)) ))
#' timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc", 
#'   doseVar = "Dose", evtVar = "Evt", iVar = "Subject") 
#' } 
#' @keywords hplot
#' @exportMethod timeEventSPlot

timeEventSPlot <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, 
		layout = NULL, maxPanels = NULL,
		problemNum = 1, subProblems = 1, xAxisScaleRelations = c("same","free","sliced"),
        yAxisScaleRelations = c("same","free","sliced"),  
        expX = FALSE, expY = FALSE, xRotAngle = 0,
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
		title = NULL, xLab = NULL, yLab = NULL,
		layout = NULL, maxPanels = NULL,
		problemNum = 1, subProblems = 1, xAxisScaleRelations = c("same","free","sliced"),
        yAxisScaleRelations = c("same","free","sliced"),  
        expX = FALSE, expY = FALSE, xRotAngle = 0,
		...) 
{
	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(timeEventSPlot, x[-1])
	
}

timeEventSPlot.NMProblem <- function(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT", 
		evtVar = "EVID", iVar = "ID",  subjectNum = NULL,
		title = NULL, xLab = NULL, yLab = NULL, 
		layout = NULL, maxPanels = NULL,
		problemNum = 1, subProblems = 1,
        xAxisScaleRelations = c("same","free","sliced"),
        yAxisScaleRelations = c("same","free","sliced"),  
        expX = FALSE, expY = FALSE, xRotAngle = 0,
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
		title = NULL, xLab = NULL, yLab = NULL, layout = NULL, maxPanels = NULL, 
		problemNum = 1, subProblems = 1, 
        xAxisScaleRelations = c("same","free","sliced"),
        yAxisScaleRelations = c("same","free","sliced"),  
        expX = FALSE, expY = FALSE, xRotAngle = 0, ...) 
{
	
	obj <- applyGraphSubset(obj)
	
	if(is.null(xLab)) xLab <- "Time"
	if(is.null(yLab)) yLab <- "Concentration"
	if(is.null(title)) title <- "Concentration vs Time"
	if(is.null(subjectNum)) subjectNum <- unique(obj[[iVar]])
	if(length(maxPanels) > 0) layout <- NULL
	# ensure that maxPanels is numeric, even if empty
	else maxPanels <- numeric(0)
    # create plotting formula
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
	
    # set the scales to implement xRotAngle and *AxisScaleRelations
   
    scales <- list(x = list( rot = xRotAngle, relation = match.arg(xAxisScaleRelations) ),
            y = list(relation = match.arg(yAxisScaleRelations)))
    objModified <- obj
    if(expX)
    {
		expXVar <- try(exp(obj[,tVar]), silent = TRUE)
        if(is(expXVar, "try-error"))
        {
            # this shouldn't be possible when invoked from application front-end
            RNMGraphicsStop("Unable to exponentiate time variable")
        }
        objModified[,tVar] <- expXVar 
        
    }
    if(expY)
    {
		expYVars <- try(exp(obj[,concVar]), silent = TRUE)
        if(is(expYVars, "try-error"))
        {
            # this shouldn't be possible when invoked from application front-end
            RNMGraphicsStop("Unable to exponentiate dependent variable")
        }
       objModified[,concVar] <- expYVars
    }
    
    plt <- with(graphParams, 
            xyplot(as.formula(form), type = "l", data = objModified,
                    par.settings = list(
                            plot.symbol = plot.symbol, 
                            
                            par.xlab.text = axis.text, par.ylab.text = axis.text,
                            par.main.text = title.text, plot.line = plot.line,
                            add.line = refline, strip.background = strip.bg,  
                            layout.widths = layout.widths, layout.heights = layout.heights), 
                    strip = strip$stripfun, panel = panel.timeEventSPlot, doseInfo = doses, colNames = varMapping,
                    xlab = xLab, ylab = yLab, main = title,
                    key = plotKey , layout = layout, scales = scales, ...) )
    multiTrellis(list(plt), maxPanels = maxPanels)
    # plt
}

setGeneric("timeEventSPlot")

setMethod("timeEventSPlot", signature(obj = "data.frame"), timeEventSPlot.data.frame)
setMethod("timeEventSPlot", signature(obj = "NMRun"), timeEventSPlot.NMRun)
setMethod("timeEventSPlot", signature(obj = "NMProblem"), timeEventSPlot.NMProblem)