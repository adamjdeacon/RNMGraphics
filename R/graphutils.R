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
# TODO: this does not seem to work correctly when group is not NULL
subjectGrouping <- function(idLabels, group = NULL, superposeCol)
{
	if(!is.null(group))
	{
		grouping <- paste(group, idLabels, sep = ",")
		groupTable <- cbind(idLabels, group)
		indices <- match(unique(idLabels), idLabels)
		groupTable <- groupTable[indices,]
		cols <- rep(superposeCol, length.out = length(unique(grouping)))
		cols <- cols[groupTable[,2]]
	}
	else
	{
		grouping <- idLabels
		cols <- superposeCol[1]
	}
	return(list(grouping = grouping, colours = cols))
}