# $Rev$
# $LastChangedDate$

# TODO: logX, logY not used

.overlaidScatter <- function(obj, xVars, yVars, bVars = NULL, gVars = NULL, iVars = NULL, 
		addLegend = TRUE, addGrid = TRUE, addLoess = FALSE, titles ="", 
		logX = FALSE, logY = FALSE, idLines = FALSE, abLines = NULL,  xLab = NULL, 
		yLab = NULL, types = "p", equalAxisScales = FALSE, maxPanels = NULL, layout = NULL,
		maxTLevels, xRotAngle = 0, ...)
{
	yVars <- CSLtoVector(yVars)
	yVarsCollapsed <- paste(yVars, collapse = "+")
	
	obj <- applyGraphSubset(obj)
	
	plotFormulas <- paste(yVarsCollapsed, "~", xVars)
	varCombos <- as.matrix(expand.grid(yVarsCollapsed, xVars))
	
	numCombos <- length(plotFormulas)
	plotList <- vector(mode = "list", length = numCombos)
	
	if(!is.null(xLab))
		xLab <- rep(CSLtoVector(xLab), length.out = numCombos)
	else
		xLab <- varCombos[,2]
	if(!is.null(yLab))
		yLab <- rep(CSLtoVector(yLab), length.out = numCombos)
	else
		yLab <- varCombos[,1]
	
	# yLabs <- rep("", numCombos)
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		temp <- processTrellis(obj, bVars, maxLevels = maxTLevels, exempt = iVars)
		bVars <- temp$columns
		# coerce each "by" variable to a factor
		obj <- coerceToFactors(temp$data, bVars)
		# coerce each "by" variable to a factor
		plotFormulas <- sapply(1:numCombos, function(i) paste(plotFormulas[i], paste(bVars, collapse = "+"), sep = "|"))
	}
	repeatVars(c("addLegend", "addGrid", "addLoess", "titles", "logX", "logY", "idLines","types", "yVarsCollapsed", "equalAxisScales"),
			list(addLegend, addGrid, addLoess, titles, logX, logY, idLines ,types,yVarsCollapsed, equalAxisScales), length.out = numCombos )
	iVars <- if(!is.null(iVars)) rep(CSLtoVector(iVars), length.out = numCombos) else rep("NULL", length.out = numCombos)
	graphParams <- getAllGraphParams()

	par.settings <- mapTopar.settings(graphParams)

	for(i in seq_along(plotFormulas))
	{
		if(addLegend[1])
			plotKey <-  scatterPlotKey("Variable", yVars, type = types[i], sortLevels = FALSE)
					# list(title = "Variable", cex=.7, rows = 10, space = "right")
		else plotKey <- NULL
		idLabels <- if(iVars[i] == "NULL") NULL else rep(obj[[iVars[i]]], times = length(yVars))
		
		scales <- list(x = list(rot = xRotAngle), y = list())
		# log axes if necessary
		if(logX[i]) scales$x <- c(scales$x, list(log = "e", at = pretty(obj[[xVars[i]]])))
		if(logY[i]) scales$y <- c(scales$y, list(log = "e", at = pretty(obj[[yVars[1]]])))
		
		if (equalAxisScales[i]) scales$limits <- padLimits(range(unlist(obj[c(xVars[i], yVars)]), na.rm=T))
		
		featuresToAdd <- c("grid" = addGrid[i], "loess" = addLoess[i], "idLine" = idLines[i])
		
		plotList[[i]] <- 
				with(graphParams, 
				xyplot(as.formula(plotFormulas[[i]]), stack = TRUE,
						data = obj, panel = panel.overlaidScatter, featuresToAdd = featuresToAdd, 
						key = plotKey, main = titles[[i]], idLabels = idLabels,
						xlab = xLab[i], ylab = yLab[i], type = types[i], scales = scales,
						par.settings = par.settings, 
				strip = strip$stripfun, layout = layout, ...)
)
	}
	gridDims <- numeric(2)
	gridDims[2] <- min(3, length(plotList) )
	gridDims[1] <- ceiling(numCombos / gridDims[2])
	
	result <- multiTrellis(plotList, gridDims, maxPanels = maxPanels)
	result
}

# TODO: allow different types for each variable

panel.overlaidScatter <- function(x, y, groups, featuresToAdd =  c("grid" = FALSE, "loess" = FALSE, "idLine" = FALSE),
		subscripts = seq_along(x), type = c("p", "o", "i", "l", "t"), idLabels = NULL, ...)
{
	type <- match.arg(type)
	if(featuresToAdd["grid"])
	{
		gridOpts <- getGraphParams("grid")
		panel.grid(h = -1, v = -1, col = gridOpts$col, alpha = gridOpts$alpha, lty = gridOpts$lty, 
				lwd = gridOpts$lwd)
	}
	reflineOpts <- getGraphParams("refline")
	if(featuresToAdd["idLine"])
		panel.abline(a = 0, b = 1)
	
	if(type == "p")
		panel.superpose(x, y, subscripts = subscripts, groups = groups)
	else if(type == "i")
	{
		#TODO: fix colours here, as they ignore the different y-variables
		RNMGraphicsStopifnot(!is.null(idLabels))
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.text"), expand= TRUE)
		textopt <- getGraphParams("superpose.text")
		ltext(x, y, idLabels[subscripts], col = groupInfo$elements$col[subscripts] , cex = groupInfo$elements$cex[subscripts] , ...)
	}
	else if(type == "l")
	{
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.line"))
		panel.superpose(x, y, groups = groupInfo$grouping, type = type, subscripts = subscripts, 
				col.line = groupInfo$elements$col, lty = groupInfo$elements$lty, lwd = groupInfo$elements$lwd, ...)		
	}
	else if(type == "t")
	{
		RNMGraphicsStopifnot(!is.null(idLabels))
		textopt <- getGraphParams("superpose.text")
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.line") )
		groupInfo2 <- subjectGrouping(idLabels, groups, textopt, expand = TRUE) 
		
		ltext(x, y, idLabels[subscripts], col = groupInfo2$elements$col[subscripts] ,
				groupInfo2$elements$cex[subscripts], ...)		
		
		panel.superpose(x, y, groups = groupInfo$grouping, type = "l", 
				subscripts = subscripts, col.line = groupInfo$elements$col,
				lty = groupInfo$elements$lty , lwd = groupInfo$elements$lwd,  ...)
	}
	else if(type == "o")
	{
		
		RNMGraphicsStopifnot(!is.null(idLabels))
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.line") )

		panel.superpose(x, y, groups = groups, type = "p", subscripts = subscripts, ...)
		panel.superpose(x, y, groups = groupInfo$grouping, type = "l", subscripts = subscripts, 
				col.line = groupInfo$elements$col, lty = groupInfo$elements$lty, lwd = groupInfo$elements$lwd, ...)
	}
	if(featuresToAdd["loess"])
	{
		loessOpts <- getGraphParams("loess.line")
		# implement a try-catch just in case loess curve fails to compute correctly
		tryLoess <- try(panel.loess(x,y, col = loessOpts$col, lwd = loessOpts$lwd))
		if(inherits(tryLoess, "try-error"))
			RNMGraphicsWarning("Failed to calculate loess curve, omitting from this panel\n")
	}
}
