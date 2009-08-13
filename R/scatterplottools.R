# TODO: Add comment
# 
# Author: fgochez
###############################################################################


scatterPlotKey <- function(gVar, gVarValues, type = c("p", "i", "t", "l", "o"))
{
	graphParams <- getAllGraphParams()
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
		result <- c(list(text = list(as.character(sort(gVarLevels)))), x)
	}
	# else we have a plot type that uses identifier text
	else
	{
		result <- with(graphParams,
					{ 
						textCol = rep(superpose.text$col, length.out = numLevels)
						lineCol = rep(superpose.line$col, length.out = numLevels)
						lineStyle = rep(superpose.line$lty, length.out = numLevels)
						x <- list(space = "right", text =  list(as.character(sort(gVarLevels)), 
						col = textCol ),title = gVar, cex = 0.7, rows = 10)
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

padLimits <- function(range, amount = 0.05) {
	magnitude = diff(range) * amount
	c(range[1] - magnitude , range[2] + magnitude )
}