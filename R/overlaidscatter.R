
.overlaidScatter <- function(obj, xVars, yVars, bVars = NULL, gVars = NULL, iVars = NULL, 
		addLegend = TRUE, addGrid = TRUE, addLoess = FALSE, titles ="", 
		logX = FALSE, logY = FALSE, idLines = FALSE, abLines = NULL,  xLab = NULL, 
		yLab = NULL, types = "p", equalAxisScales = FALSE, maxPanels = NULL, layout = NULL,
		maxTLevels, xRotAngle = 0, graphParams = getAllGraphParams(), xAxisScaleRelations, 
        yAxisScaleRelations, ...)
{
	yVars <- CSLtoVector(yVars)
	yVarsCollapsed <- paste(yVars, collapse = "+")
	
	dataSet <- applyGraphSubset(obj)
	
	xVars <- CSLtoVector(xVars)
	if (all(length(xVars) != 1)) { stop("multiple x-variables not currently implemented") }
		
	plotFormulas <- paste(yVarsCollapsed, "~", xVars)
	varCombos <- as.matrix(expand.grid(yVarsCollapsed, xVars))
	
	numCombos <- length(plotFormulas)
	plotList <- vector(mode = "list", length = numCombos)
	
	if(!is.null(xLab))
		xLab <- rep(xLab, length.out = numCombos)
	else
		xLab <- varCombos[,2]
	if(!is.null(yLab))
		yLab <- rep(yLab, length.out = numCombos)
	else
		yLab <- varCombos[,1]
	
	# yLabs <- rep("", numCombos)
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		temp <- processTrellis(dataSet, bVars, maxLevels = maxTLevels, exemptColumns = iVars)
		bVars <- temp$columns
		# coerce each "by" variable to a factor
		dataSet <- coerceToFactors(temp$data, bVars)
		# coerce each "by" variable to a factor
		plotFormulas <- sapply(seq_len(numCombos), function(i) paste(plotFormulas[i], paste(bVars, collapse = "+"), sep = "|"))
	}
	repeatVars(c("addLegend", "addGrid", "addLoess", "titles", "logX", "logY", "idLines","types", "yVarsCollapsed", "equalAxisScales"),
			list(addLegend, addGrid, addLoess, titles, logX, logY, idLines ,types,yVarsCollapsed, equalAxisScales), length.out = numCombos )
	
	iVars <- if(!is.null(iVars)) { CSLtoVector(iVars) }
	
	iVars <- iVars[iVars %in% colnames(dataSet)]
	
	if (length(iVars) == 0) { iVars <- NULL }
	
	par.settings <- mapTopar.settings(graphParams)
	for(i in seq_along(plotFormulas))
	{
		if(addLegend[1])
		{
			plotKey <-  scatterPlotKey("Variable", yVars, type = ifelse(!addLoess[1], types[i], "o"), 
				 sortLevels = FALSE, graphParams = graphParams)
				 # list(title = "Variable", cex=.7, rows = 10, space = "right")
			if(addLoess[1] & (types[i] %in% c("p", "i", "t", "l"))) 
			{
					if((types[i] %in% c("i", "t", "l")) & ("points" %in% names(plotKey))) 
						plotKey <- plotKey[-which(names(plotKey)=="points")]
					if((types[i] %in% c("i", "p"))) plotKey$lines$col <- graphParams$loess.line$col[1]
			}
		}  else plotKey <- NULL
		
		# if type = "i", idLabels must not be NULL
		# assuming that iVars is only ever not length 1 when there is nested grouping in iVars
		#     there will never be one iVars grouping for this plot no matter how many xVars are selected.
		#     adding this grouping will cause the panel function to plot nested grouped lines separately.
		
		pastes <- function(...) { paste(..., sep = "/") }
		
		idLabels <- if(is.null(iVars)) { NULL } else { rep(do.call("pastes", dataSet[iVars]), times = length(yVars)) }
		
		scales <- list(x = list(rot = xRotAngle, relation = xAxisScaleRelations),
                y = list(relation = yAxisScaleRelations))
		
		# log axes if necessary
		# note that this only works with a single y axis variable, unless the scatter plot is overlaid
		
		if(logX[i]) scales$x <- c(scales$x, list(log = "e", at = pretty(dataSet[[xVars[i]]])))
		if(logY[i]) scales$y <- c(scales$y, list(log = "e", at = pretty(dataSet[[yVars[1]]])))
		
		# if axis scales are matched, set the limits and pad to avoid clipping
		
		if (equalAxisScales[i])
		{
			
			# changes made in response to issue 2755
			# if we are forcing the axis scales to be identical, we must pad out the range of the data since otherwise
			# clipping occurs.  However, care must be taken if logged axes were requested	
			
			if(logX | logY) 
			{
				
				# Here we use a minimum to avoid 0 or negative axes with logging
				
				scales$limits <- padLimits(range(unlist(dataSet[,c(xVars[1],yVars)]), na.rm=TRUE), min = LOGAXISLIMIT)
				RNMGraphicsWarning("Matching scales and logging requested - axis limits will be bounded from below by 0.01 which may caused data to be clipped\n")			
			}
			else
				scales$limits <- padLimits(range(unlist(dataSet[,c(xVars[1],yVars)]), na.rm=TRUE))
			
		}
		featuresToAdd <- c("grid" = addGrid[i], "loess" = addLoess[i], "idLine" = idLines[i])
		
		plotList[[i]] <- 
				with(graphParams, 
				xyplot(as.formula(plotFormulas[[i]]),
						data = dataSet, panel = panel.overlaidScatter, featuresToAdd = featuresToAdd, 
						key = plotKey, main = titles[[i]], idLabels = idLabels,
						xlab = xLab[i], ylab = yLab[i], type = types[i], scales = scales,
						par.settings = par.settings, 
				strip = strip$stripfun, layout = layout, graphParams = graphParams, abLines = abLines, ...)
)
	}
	gridDims <- numeric(2)
	gridDims[2] <- min(3, length(plotList) )
	gridDims[1] <- ceiling(numCombos / gridDims[2])
	
	result <- multiTrellis(plotList, gridDims, maxPanels = maxPanels)
	result
}

#' @details panel.overlaidScatter is the nmScatterPlot panel function (2 of 2) used when overlaid y axis variables are requested.
#' @name panel functions
#' @aliases panel.nmBoxPlot panel.nmHistogram panel.overlaidScatter prepanel.nmBoxPlot
#' @param x (usual)
#' @param y (usual)
#' @param subscripts (usual) 
#' @param featuresToAdd named logical vector of features to add (grid, loess, idLine) 
#' @param idLabels Identifier labels 
#' @param type one of "p", "i", "l", "o", "t". Note that for "l" (lines connected by the variable specified in "iVars"), 
#' "o" (lines and points), and "t" (labels connected by lines grouped by "iVars") lines will be added independently within nested groups.
#' @param groups (usual)
#' @param graphParams Full list of RNMGraphics graphical settings
#' @param ... additional parameters to panel.xyplot / panel.superpose
#' @examples 
#' \dontrun{ 
#' df1 <- data.frame(X = rep(1:10, times = 2), Y1 = c(2:11, 3:12)^0.8, 
#'     Y2 = c(3:12, 4:13)^0.6, G = rep(letters[1:2], each = 10))
#' xyplot(Y1 + Y2 ~ X | G, data = df1,
#'     panel = RNMGraphics:::panel.overlaidScatter, type = "o", idLabels = df1$G, 
#'     graphParams = getAllGraphParams())
#' df1$N <- rep(1:2, times = 5)
#' df1 <- df1[with(df1, order(G, N, X)), ]
#' xyplot(Y1 + Y2 ~ X | G, data = df1,
#'     panel = RNMGraphics:::panel.overlaidScatter, type = "o", idLabels = df1$G, 
#'     graphParams = getAllGraphParams())
#' }

panel.overlaidScatter <- function(x, y, groups, featuresToAdd =  c("grid" = FALSE, "loess" = FALSE, "idLine" = FALSE),
		subscripts = seq_along(x), type = c("p", "o", "i", "l", "t"), idLabels = NULL, 
		graphParams, abLines = NULL, ...)
{
	type <- match.arg(type)
	if(featuresToAdd["grid"])
	{
		gridOpts <- graphParams$grid
		panel.grid(h = -1, v = -1, col = gridOpts$col, alpha = gridOpts$alpha, lty = gridOpts$lty, 
				lwd = gridOpts$lwd)
	}
	reflineOpts <- graphParams$"refline"
	if(featuresToAdd["idLine"])
		panel.abline(a = 0, b = 1)
	
	if(type == "p")
		panel.superpose(x, y, subscripts = subscripts, groups = groups)
	else if(type == "i")
	{
		#TODO: fix colours here, as they ignore the different y-variables
		RNMGraphicsStopifnot(!is.null(idLabels))
		groupInfo <- subjectGrouping(idLabels, groups, graphParams$"superpose.text", expandElements= TRUE)
		textopt <- graphParams$"superpose.text"
		ltext(x, y, idLabels[subscripts], col = groupInfo$elements$col[subscripts] , cex = groupInfo$elements$cex[subscripts] , ...)
	}
	else if(type == "l")
	{
		groupInfo <- subjectGrouping(idLabels, groups, graphParams$"superpose.line")
		panel.superpose(x, y, groups = groupInfo$grouping, type = type, subscripts = subscripts, 
				col.line = groupInfo$elements$col, lty = groupInfo$elements$lty, lwd = groupInfo$elements$lwd, ...)		
	}
	else if(type == "t")
	{
		RNMGraphicsStopifnot(!is.null(idLabels))
		textopt <- graphParams$superpose.text
		groupInfo <- subjectGrouping(idLabels, groups, graphParams$superpose.line )
		groupInfo2 <- subjectGrouping(idLabels, groups, textopt, expandElements = TRUE) 
		
		ltext(x, y, idLabels[subscripts], col = groupInfo2$elements$col[subscripts] ,
				groupInfo2$elements$cex[subscripts], ...)		
		
		panel.superpose(x, y, groups = groupInfo$grouping, type = "l", 
				subscripts = subscripts, col.line = groupInfo$elements$col,
				lty = groupInfo$elements$lty , lwd = groupInfo$elements$lwd,  ...)
	}
	else if(type == "o")
	{
		
		RNMGraphicsStopifnot(!is.null(idLabels))
		groupInfo <- subjectGrouping(idLabels, groups, graphParams$superpose.line )

		panel.superpose(x, y, groups = groups, type = "p", subscripts = subscripts, ...)
		panel.superpose(x, y, groups = groupInfo$grouping, type = "l", subscripts = subscripts, 
				col.line = groupInfo$elements$col, lty = groupInfo$elements$lty, lwd = groupInfo$elements$lwd, ...)
	}
	if(featuresToAdd["loess"])
	{
		loessOpts <- graphParams$loess.line
		subGroups <- groups[subscripts]
		groupInfo <- unique(subGroups)
		# implement a try-catch just in case loess curve fails to compute correctly
		for(i in 1:length(groupInfo))
			tryLoess <- try(panel.loess(x[subGroups==groupInfo[i]], y[subGroups==groupInfo[i]], 
				lty =graphParams$superpose.line$lty[i], col = loessOpts$col[1], lwd = loessOpts$lwd))
		if(inherits(tryLoess, "try-error"))
			RNMGraphicsWarning("Failed to calculate loess curve, omitting from this panel\n")
	}

    if(!is.null(abLines))
    {
        # check that abLines is a list of vectors of length 1 or 2
        if(!(is(abLines, "list") & all(sapply(abLines, length) %in% c(1,2))))
        {
            RNMGraphicsWarning("Reference line parameter (abLines) is invalid")
            return()
        }
        
        for(x in abLines)
        {
            tryCatch(panel.abline(x), 
                    error = function(e) RNMGraphicsWarning(paste("Failed to plot a reference line, error message is:", e)))
        }
        
    }
}
