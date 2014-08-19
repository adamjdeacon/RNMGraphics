

#' safe version of getVarLabel - extracts the label associated to a variable, unless there is none
#' in which case the name of the variable replaces it
#' @name Get variable label
#' @title Get variable label
#' @param varName Name of NONMEM variable
#' @param useVarNameifMissing If TRUE, returns the variable name as the label if the variable name does not have
#' a label, else returns an empty character
#' @param maxLength : if label is longer than this, insert line-feeds to break
#' @return variable label, variable name or character(0) depending on parameters
#' @author Mango Solutions
#' @keywords variable
#' @noRd

# TODO: unit test

getVarLabel <- function(varName, useVarNameifMissing = TRUE, maxLength = getGraphParams("legend")$maxTitleLength)
{
	lab <- getVarDescription(varName)$Label
	if(length(lab) == 0 && useVarNameifMissing)
		lab <- varName
	# handle long names by splicing in line feeds 
	if(nchar(lab) > maxLength)
	{
		splitPoints <- seq(from = 0, to = nchar(lab), by = maxLength )
		substrings <- substring(lab, head(splitPoints, -1) + 1,tail(splitPoints, -1 ))
		# add remaining chunk
		if(nchar(lab) %% maxLength > 0)
			substrings <- c(substrings, substring(lab, tail(splitPoints, 1) + 1, nchar(lab)))
		lab <- paste(substrings, collapse = "\n")
	}
	lab
}

#' (currently deprecated)
#' @title grid dimensions
#' @param numPlots 
#' @param maxColumns 
#' @return numeric
#' @author Mango Solutions
#' @keywords grid
#' @noRd

stdGridDims <- function(numPlots, maxColumns )
{
	gridDims <- numeric(2)
	gridDims[2] <- min(3, numPlots )
	gridDims[1] <- ceiling(numPlots / gridDims[2])

	gridDims
}

#' Generates a matrix of combinations of one set of set of variables "against" another set
#' @title Generates a matrix of combinations for variables
#' @param xVars Vector or comma seperated list of x variable names
#' @param yVars Vector or comma seperated list of y variable names
#' @param collapseY Collapse Y variables using +? (meant to be used with extended lattice formulas)
#' @param collapseX Collapse X variables using +? meant to be used with extended lattice formulas)
#' @return matrix of all xVars against yVars
#' @author Mango Solutions
#' @keywords combinations
#' @noRd

varComboMatrix <- function(xVars, yVars, collapseY = TRUE, collapseX = FALSE)
{
	xVars <- CSLtoVector(xVars); yVars <- CSLtoVector(yVars)
	if(collapseY) yVars <- paste(yVars, collapse = "+")
	if(collapseX) xVars <- paste(xVars, collapse = "+")
	as.matrix(expand.grid(xVars, yVars))
}


#' Utility function that generates groupings for use with scatter plot types = l, i, o and t.
#' Takes account of the fact that the subject identifier groupings should not vary line/text etc. colours, 
#' @title Generate subject groupings
#' @param idLabels subject identifier labels (e.g. from iVar) 
#' @param group Additional grouping variable
#' @param superposeElements list of styles which should be generated for each group (colours, line types, line widths etc)
#' @param expandElements Should the superpose elements be reproduced for each element in the grouping, or each group level?
#' @return a list of the grouping elements and the grouping variable that combines the identifier grouping with the other groups 
#' @author Mango Solutions
#' @keywords groupings
#' @noRd

subjectGrouping <- function(idLabels, group = NULL, superposeElements, expandElements = FALSE)
{
	if(!is.null(group))
	{
		# create combined grouping variable
		
		grouping <- paste(group, idLabels, sep = ",")
		groupTable <- cbind(as.factor(group), idLabels)
		
		# repeat for each element of the grouping, rather than group levels
		if(!expandElements)
		{
			indices <- match(unique(grouping), grouping)
			groupTable <- groupTable[indices,]
			elts <- lapply(superposeElements, rep, length.out = length(unique(group)))
		
			elts <- lapply(elts, function(x) x[as.numeric(groupTable[,1])])
		
		}
		else
		{
			elts <- lapply(superposeElements, rep, length.out = length(group))
			elts <- lapply(elts, function(x) x[as.numeric(groupTable[,1])])
		}
	}
	else
	{
		grouping <- idLabels
		elts <- lapply(superposeElements, function(x) x[1])
	}
	return(list(grouping = factor(grouping, ordered = TRUE, levels = unique(grouping)), elements = elts))
}

# The default package strip function. This strip function is needed since if multiple y variables are used yet no
# "by" variable is, a crash occurs if strip.names = c(TRUE, TRUE)


defaultStrip <- function(..., var.name)
{
	if(is.null(var.name)) strip.names = c(FALSE, TRUE) else strip.names = c(TRUE, TRUE)
	strip.default(..., var.name = var.name, strip.names = strip.names)
}

# a second default strip function, to be used in a later release of RNMGraphics

.defaultStrip <- function(..., sep, var.name)
{
	strip.names <- c(TRUE, TRUE)
	sep <- ":"
	if(is.null(var.name)) {
		strip.names <- c(FALSE, TRUE)
		sep <- ""
	}
	else if(var.name == "NSIM")
	{
		var.name <- ""
		sep <- ""
	}
	strip.default(..., var.name = var.name, strip.names = strip.names, 
			sep = sep)
	
}

#' reassigns panel layout based on a maximum number of panels.
#' @title Calculate Max Panels
#' @param obj 
#' @param maxPanels single numeric
#' @return obj 
#' @author Mango Solutions
#' @keywords panels
#' @noRd
calcMaxPanels <- function(obj, maxPanels = 8) 
{
	if (length(maxPanels) != 1 || maxPanels[1] < 1) RNMGraphicsStop("Illegal maxPanels value")	
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

