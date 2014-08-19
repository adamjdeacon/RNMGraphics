
#' Instantiates a multi-trellis class object
#'
#' NOTE: Originally, it was envisioned that multiple y (or x) axis variables would be plotted
#' across multiple plots stored in a single "multiTrellis" object.  Each x/y combo would be stored 
#' as a seperate plot.  However, the underlying architecture was changed so that the extended lattice
#' formula interface would be used.  Thus the ability to have multiple plots in a single object is currently
#' deprecated, and will not be exposed for now
#' 
#' @param plotSet A list of trellis plots.  Currently, it should be of length 1 
#' @param gridDims (not implemented)
#' @param mainTitle (not implemented)
#' @param panelLayout layout (# of rows and columns) of panels of each plot.  Can be a length 2 numeric vector
#' or a length 0 vector, in which case it is not used
#' @param gridLayout (not implented)
#' @param maxPanels [N,1] - Maximum number of panels to display on each page 
#' @title Multi trellis object constructor
#' @return Multi-trellis object holding plots
#' @usage multiTrellis(plotSet, gridDims = c(1,1), mainTitle = "", 
#'		panelLayout = getGraphParams("panelLayout")\$layout, gridLayout = numeric(0), 
#'		maxPanels = numeric(0))
#' @keywords trellis
#' @author fgochez
#' @exportClass multiTrellis

multiTrellis <- function(plotSet, gridDims = c(1,1), mainTitle = "", 
		panelLayout = getGraphParams("panelLayout")$layout, gridLayout = numeric(0), 
		maxPanels = numeric(0))
{
	if(length(plotSet) > 1)
	{
		RNMGraphicsWarning("Only one plot currently supported in multiTrellis objects, will subset")
		plotSet <- plotSet[1]
		gridDims <- c(1,1)
	}
	
	new("multiTrellis", plots = plotSet, 
			layout = gridDims, mainTitle = mainTitle, 
			panelLayout = panelLayout, maxPanels = maxPanels)
}

validity.multiTrellis <- function(object)
{
	
	if(length(object@layout) != 2)
		return("Layout dimensions incorrect")
	if(length(object@plots) > 1 )
		return("At the moment, only one plot is allowed")

	TRUE
}



setClass("multiTrellis", representation(plots = "list", layout = "numeric", 
				mainTitle = "character", panelLayout = "numeric", maxPanels = "numeric"), 
		validity = validity.multiTrellis)

# special case for a single plot

plotSingletonmultiTrellis <- function(object)
{
	# TODO: remove this
	if(length(object@panelLayout) > 0)
		object@plots[[1]]$layout <- object@panelLayout
	as.table <- getGraphParams("panelMisc")$as.table
	# TODO: this is currently a hack.  Pass as.table as a parameter later.
	if(!is.na(as.table))
		object@plots[[1]]$as.table <- as.table
	if(length(object@maxPanels) > 0)
	{
		object@plots[[1]] <- calcMaxPanels(object@plots[[1]], object@maxPanels)
	}
	print(object@plots[[1]])
}

# Currently unused

.show.multiTrellis <- function(object)
{
	if(length(object@plots) == 1)
	{
		plotSingletonmultiTrellis(object)
		return()
		
	}
	grid.newpage()
	gridDims <- object@layout
	if(length(object@panelLayout) > 0) 
		RNMGraphicsStop("Panel layouts not yet supported for multiple plots")
		# objec
	newpage <- FALSE
	if(!newpage)
		pushViewport(viewport(layout = 	grid.layout(gridDims[1], gridDims[2] )))
	
	k <- 1
	for(i in 1:gridDims[1]) 
	{
		j <- 1
		while(j <= gridDims[2] & k <= length(object@plots))
		{
			
			if(length(object@panelLayout) > 0)
				object@plots[[k]]$layout <- object@panelLayout 
				# object
			if(!newpage)
				pushViewport(viewport(layout.pos.col=j, layout.pos.row=i, 
							width = unit(1.5, "npc")))
			# TODO : Determination of a new page needs to be more sophisticated
			print(object@plots[[k]], newpage = newpage)
			if(!newpage)
				popViewport(1)
			j <- j + 1
			k <- k + 1
		}
	}
}

setMethod("show", signature(object = "multiTrellis"), plotSingletonmultiTrellis)

#' assigns x to slot \code{panelLayout}
#' @title Assign to panelLayout
#' @param x
#' @param value
#' @return x
#' @author fgochez
#' @keywords panels
# TODO: input validity checking

"panelLayout<-" <-function(x, value)
{
	x@panelLayout <- value
	x
}