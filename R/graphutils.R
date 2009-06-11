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

sortByGroupings <- function(x, iVar, gVar = NULL)
{
	
}

# TODO: this does not seem to work correctly when group is not NULL
subjectGrouping <- function(idLabels, group = NULL, superposeCol)
{
	if(!is.null(group))
	{
		grouping <- paste(group, idLabels, sep = ",")
		groupTable <- cbind(as.factor(group), idLabels)
		indices <- match(unique(idLabels), idLabels)
		groupTable <- groupTable[indices,]
		cols <- rep(superposeCol, length.out = length(unique(group)))
		cols <- cols[as.numeric(groupTable[,1])]
	}
	else
	{
		grouping <- idLabels
		cols <- superposeCol[1]
	}
	return(list(grouping = grouping, colours = cols))
}

# this strip function is needed since if multiple y variables are used yet no
# "by" variable is, a crash occurs if strip.names = c(TRUE, TRUE)

defaultStrip <- function(..., var.name)
{
	if(is.null(var.name)) strip.names = c(FALSE, TRUE) else strip.names = c(TRUE, TRUE)
	strip.default(..., var.name = var.name, strip.names = strip.names)
}

calcMaxPanels <- function(obj, maxPanels = 8) 
{
	if (length(maxPanels) != 1 || maxPanels[1] < 1) stop("Illegal maxPanels value")	
	if (length(obj$layout) | !length(obj$condlevels)) return(obj)
	nLats <- length(cl <- obj$condlevels)
	cLens <- sapply(cl, length)
	totalPanels <- prod(cLens)
	if (totalPanels <= maxPanels) return(obj)
	maxPanels <- min(maxPanels, cLens[1])
	switch(as.character(nLats), 
			"0" = obj$layout <- NULL, 
			"1" = {
				obj$layout <- lattice:::compute.layout(NULL, maxPanels) 
				obj$layout[3] <- ceiling(totalPanels / obj$layout[2])
			}, {
				firstLevel <- 1:cLens[1]
				obj$layout <- lattice:::compute.layout(NULL, maxPanels)
				if (length(obj$layout) && obj$layout[1] == 0) {
					m <- max(1, round(sqrt(obj$layout[2])))
					n <- ceiling(obj$layout[2]/m)					
					m <- ceiling(obj$layout[2]/n)
					obj$layout[1] <- n
					obj$layout[2] <- m
				}
				
				firstPages <- ceiling(cLens[1]/prod(obj$layout[1:2]))
				obj$layout[3] <- firstPages * prod(cLens[-1])
				firstPanels <- firstPages * obj$layout[1] * obj$layout[2]
				if (firstPanels > cLens[1]) {
					
					# Need to skip extra spaces
					
					obj$skip <- rep(1:firstPanels > cLens[1], prod(cLens[-1]))
				}
			})
	obj
	
}

