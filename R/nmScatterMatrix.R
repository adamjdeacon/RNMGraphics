# $Rev$
# $LastChangedDate$


#' Generates a scatterplot matrix of a set of variables from a PK/PD dataset
#' @name nmScatterMatrix
#' @title NONMEM scatter-plot matrix
#' @param obj object of class NMProblem, NMRun or data.frame
#' @param vars Character vector or comma seperated list of variable names to plot
#' @param bVars character vector or comma-seperated list of trellis variables
#' @param addLoess Logical flag. Should a loess smoother curve be added to the scatter-plots? 
#' @param title Plot's main title
#' @param ... Additional parameters passed to the splom function
#' @return An object of class multiTrellis 
#' @author fgochez
#' @keywords hplot

nmScatterMatrix <- function(obj, vars,bVars = NULL,  addLoess = FALSE, title ="", ...)
{
	RNMGraphicsStop("Not implemented for this class yet!")
}

setGeneric("nmScatterMatrix")

# TODO: ability to add L curve

nmScatterMatrix.data.frame <- function(obj, vars,bVars = NULL,  addLoess = FALSE, title = "", ...)
{
	vars <- CSLtoVector(vars)
	vars <- paste("'", vars, "'", sep = "")
	numCombos <- length(vars)
	
	#repeatVars(c("addLoess"), list(addLoess), 
	#		length.out = numCombos)
	# only one allowed at the moment
	plotFormulas <- paste(" ~ obj[c(", paste(vars, collapse = ","), ")]")
	
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		obj <- coerceToFactors(obj, bVars)
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "+"), sep = "|") 
	}	
	graphParams <- getAllGraphParams()
	plt <- splom(as.formula(plotFormulas), data = obj, main = title, panel = panel.nmScatterMatrix, addLoess = addLoess, 
			par.settings = list(par.xlab.text = graphParams$"axis.text", 
					par.ylab.text = graphParams$"axis.text", par.main.text = graphParams$title.text,
					plot.symbol = graphParams$plot.symbol, strip.background = graphParams$strip.bg),
			...)
	multiTrellis(list(plt))
		
}

setMethod("nmScatterMatrix", signature(obj = "data.frame"), nmScatterMatrix.data.frame)

panel.nmScatterMatrix <- function(x,y, addLoess = FALSE, ...)
{
	panel.xyplot(x,y,...)
	if(addLoess)
	{
		loessopt <- getGraphParams("loess.line")
		loessTry <- try(with(loessopt, panel.loess(x, y, col = col, lwd = lwd) ))
		if(inherits(loessTry, "try-error")) RNMGraphicsWarning("A call to panel.loess failed, so the smoother will be omitted.\n")
	}
}