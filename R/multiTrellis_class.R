# TODO: Add comment
# 
# Author: fgochez
# $Rev$
# $LastChangedDate$
#
###############################################################################


multiTrellis <- function(plotSet, gridDims = c(1,1), mainTitle = "", 
		panelLayout = getGraphParams("panelLayout")$layout, gridLayout = numeric(0), 
		maxPanels = numeric(0))
{
	new("multiTrellis", plots = plotSet, 
			layout = gridDims, mainTitle = mainTitle, 
			panelLayout = panelLayout, maxPanels = maxPanels)
}

# TODO: finish

validity.multiTrellis <- function(object)
{
	
	if(length(object@layout) != 2)
		return("Layout dimensions incorrect")
	if(length(object@plots) > object@layout[1] * object@layout[2])
		return("Number of plots and layout dimensions are not compatible")

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

# TODO: this is becoming convoluted

show.multiTrellis <- function(object)
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

setMethod("show", signature(object = "multiTrellis"), show.multiTrellis)

# TODO: input validity checking

"panelLayout<-" <-function(x, value)
{
	x@panelLayout <- value
	x
}