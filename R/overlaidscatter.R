# $Rev$
# $LastChangedDate$

# TODO: bVars not working yet
# TODO: logX, logY not used
# TODO: take out "doPlot"

.overlaidScatter <- function(obj, xVars, yVars, bVars = NULL, gVars = NULL, iVars = NULL, 
		addLegend = TRUE, addGrid = TRUE, addLoess = FALSE, titles ="", 
		logX = FALSE, logY = FALSE, idLines = FALSE, abLines = NULL,  xLab = NULL, 
		yLab = NULL,  doPlot=FALSE, types = "p", equalAxisScales = FALSE, ...)
{
	yVars <- CSLtoVector(yVars)
	yVarsCollapsed <- paste(yVars, collapse = "+")
	gVars <- if(!is.null(gVars)) CSLtoVector(gVars)[1] else "NULL"
	
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
		obj <- coerceToFactors(obj, bVars)
		# coerce each "by" variable to a factor
		plotFormulas <- sapply(1:numCombos, function(i) paste(plotFormulas[i], paste(bVars, collapse = "+"), sep = "|"))
	}
	repeatVars(c("addLegend", "addGrid", "addLoess", "titles", "logX", "logY", "idLines","types", "yVarsCollapsed", "equalAxisScales"),
			list(addLegend, addGrid, addLoess, titles, logX, logY, idLines ,types,yVarsCollapsed, equalAxisScales), length.out = numCombos )
	iVars <- if(!is.null(iVars)) rep(CSLtoVector(iVars), length.out = numCombos) else rep("NULL", length.out = numCombos)
	graphParams <- getAllGraphParams()
	for(i in seq_along(plotFormulas))
	{
		if(addLegend[1])
			plotKey <- list(title = "Variable", cex=.7, columns = 3)
		else plotKey <- NULL
		idLabels <- if(iVars[i] == "NULL") NULL else rep(obj[[iVars[i]]], times = length(yVars))
		
		scales <- list()
		# log axes if necessary
		if(logX[i]) scales$x <- list(log = "e", at = pretty(obj[[xVars[i]]]))
		if(logY[i]) scales$y <- list(log = "e", at = pretty(obj[[yVars[1]]]))
		# TODO: this will not quite work when there are multiple yVars
		if (equalAxisScales[i]) scales$limits <- range(unlist(dataSet[c(xVars[i], yVars)]), na.rm=T)
		
		featuresToAdd <- c("grid" = addGrid[i], "loess" = addLoess[i], "idLine" = idLines[i])
		
		plotList[[i]] <- 
				with(graphParams, 
				xyplot(as.formula(plotFormulas[[i]]), stack = TRUE,
						data = obj, panel = panel.overlaidScatter, featuresToAdd = featuresToAdd, 
						auto.key = plotKey, main = titles[[i]], idLabels = idLabels,
						xlab = xLab[i], ylab = yLab[i], type = types[i], scales = scales,
						par.settings = list(
						superpose.symbol = superpose.symbol,
						par.xlab.text = axis.text, par.ylab.text = axis.text,
						par.main.text = title.text, plot.line = plot.line,
						add.line = refline, strip.background = strip.bg), 
				strip = strip$stripfun, ...)
)
	}
	gridDims <- numeric(2)
	gridDims[2] <- min(3, length(plotList) )
	gridDims[1] <- ceiling(numCombos / gridDims[2])
	
	result <- multiTrellis(plotList, gridDims)
	result
}

# TODO: implement different plot types

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
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.text")$col)
		textopt <- getGraphParams("superpose.text")
		ltext(x, y, idLabels[subscripts], col = groupInfo$colours , cex = textopt$cex , ...)		
	}
	else if(type == "l")
	{
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.line")$col )
		panel.superpose(x, y, groups = groupInfo$grouping, type = type, subscripts = subscripts, 
				col.line = groupInfo$colours, ...)	
		
	}
	else if(type == "t")
	{
		RNMGraphicsStopifnot(!is.null(idLabels))
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.line")$col )
		if(!is.null(groups)) 
		{
			textopt <- getGraphParams("plot.text")
			ltext(x, y, idLabels[subscripts], col = textopt$col , cex = textopt$cex , ...)		
			panel.superpose(x, y, groups = groupInfo$grouping, type = "l", 
					subscripts = subscripts, col.line = groupInfo$colours,	, ...)
		}
		else
		{
			textopt <- getGraphParams("plot.text")
			ltext(x, y, idLabels[subscripts], col = textopt$col , cex = textopt$cex , ...)
			panel.superpose(x = x, y = y, subscripts = subscripts, type = "l", 
					groups = groupInfo$grouping, col.line = groupInfo$colours, ...) 
		}
	}
	else if(type == "o")
	{
		RNMGraphicsStopifnot(!is.null(idLabels))
		groupInfo <- subjectGrouping(idLabels, groups, getGraphParams("superpose.line")$col )
		groupInfo2 <- subjectGrouping(idLabels, groups, getGraphParams("superpose.symbol")$col )
		panel.superpose(x, y, groups = groupInfo$grouping, 
				type = type, subscripts = subscripts, col.line = groupInfo$colours, 
				col.symbol = groupInfo2$colours, ...) 			
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
