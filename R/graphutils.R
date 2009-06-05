# TODO: Add comment
# $Rev$
# $LastChangedDate$
# Author: fgochez
###############################################################################


stdGridDims <- function(numPlots, maxColumns )
{
	gridDims <- numeric(2)
	gridDims[2] <- min(3, numPlots )
	gridDims[1] <- ceiling(numPlots / gridDims[2])

	gridDims
}

varComboMatrix <- function(xVars, yVars, collapseY = TRUE, collapseX = FALSE)
{
	xVars <- CSLtoVector(xVars); yVars <- CSLtoVector(yVars)
	if(collapseY) yVars <- paste(yVars, collapse = "+")
	if(collapseX) xVars <- paste(xVars, collapse = "+")
	as.matrix(expand.grid(xVars, yVars))
}