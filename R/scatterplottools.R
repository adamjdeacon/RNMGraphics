
#' Generates a scatter plot legend for use with nmScatterPlot
#' @title Generate scatter plot legend
#' @param gVar Name of the grouping variable (or the word "Variable" when the key is generated for a plot with overlaid
#' y-axis variables
#' @param gVarValues Values of the grouping variable, or names of the variables
#' @param type Scatter plot type
#' @param sortLevels Should the "grouping levels" be sorted?  This is meant to be used when the scatter plot key is
#' being generated for a plot with overlaid y-axis variables, rather than for a plot used with a grouping variable 
#' @return a list to be passed in as a scatter-plot legend 
#' @author Mango Solutions
#' @noRd

scatterPlotKey <- function(gVar, gVarValues, type = c("p", "i", "t", "l", "o"), 
							sortLevels = TRUE, graphParams = getAllGraphParams())
{
	gVarLevels <- unique(gVarValues)
	numLevels <- length(gVarLevels)
	RNMGraphicsStopifnot(numLevels > 0)
	# list(title = "Variable", cex=.7, rows = 10, space = "right")
	type <- match.arg(type)
	# handle those that don't use text first
	if(type %in% c("p", "l", "o")) {
		points <- list()
		
		x <-with(graphParams, { 
				pointCol <- rep(superpose.symbol$col, length.out = numLevels)
				pointPch <- rep(superpose.symbol$pch, length.out = numLevels)
				lineCol <- rep(superpose.line$col, length.out = numLevels)
				lineStyle <- rep(superpose.line$lty, length.out = numLevels)
				
				switch( type,
					"p" = {
						list(points = list(col = pointCol, pch = pointPch))
					},
					"o" = {
						list(points = list(col = pointCol, pch = pointPch), 
								lines = list(col = lineCol, lty = lineStyle))
					},
					"l" = {
						list(lines = list(col = lineCol, lty = lineStyle))
					})
			}
		)
		x$rows <- 10
		x$cex <- 0.7
		x$space <- "right"	
		x$title <- gVar
		# x$text <- list(as.character(sort(gVarLevels)))
		if(sortLevels)
			result <- c(list(text = list(as.character(sort(gVarLevels)))), x)
		else
			result <- c(list(text = list(as.character(gVarLevels))), x)
	}
	# else we have a plot type that uses identifier text
	else
	{
		result <- with(graphParams,
					{ 
						textCol = rep(superpose.text$col, length.out = numLevels)
						lineCol = rep(superpose.line$col, length.out = numLevels)
						if(sortLevels) legendLabels <- as.character(sort(gVarLevels)) else legendLabels <- as.character(gVarLevels)
						lineStyle = rep(superpose.line$lty, length.out = numLevels)
						x <- list(space = "right", text =  list(legendLabels,col = textCol ),
								title = gVar, cex = 0.7, rows = 10)
						if(type == "t") x$lines <- list(col = lineCol, lty = lineStyle)
						x
					}
				)
	}
	result
}

# utility function
# pads out the limits of scatter plot when equalAxisScales = TRUE.  Without this padding, clipping of plot points
# occurs at the edges of the plot area
# padding cannot exceed the limits.  This is needed for padding logged axes

padLimits <- function(range, amount = 0.05, min = -Inf, max = Inf) {
	magnitude = diff(range) * amount
	
	newLimits <- c(range[1] - magnitude , range[2] + magnitude )
	newLimits[1] <- if(newLimits[1] < min) min else newLimits[1]
	newLimits[2] <- if(newLimits[2] > max) max else newLimits[2]
	newLimits
}


# Returns a data.frame with any rows containing Inf or -Inf removed
# @param df A data.frame
# @title Data frame with rows containing infinitites removed
# @return A data.frame

strippedValRows <- function(df, vals)
{
	if(nrow(df) == 0)
		return(df)
	if(ncol(df) == 0)
		return(df)
	
	rowsWithInf <- apply( df, 1, function(x) any( x %in% vals ) )
	
	df[!rowsWithInf , ]
}