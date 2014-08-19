
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
#' @author Mango Solutions
#' @exportClass multiTrellis
#' @noRd 
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


#' @title Class "multiTrellis".
#'
#' @description A class designed to hold multiple lattice plots and their layout on a single device page.
#' At the moment, this is mostly unimplemented and only a single plot is stored. 
#'
#' @section Objects from the Class:
#' 	Objects of this class are typically returned by plotting routines in this package.  They can also be 
#' 	created via the \code{multiTrellis} function, though this is not meant to be done directly.
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{plots}:}{List of plots to include in the multiTrellis object.  Currently only one plot is allowed.}
#'
#'    \item{\code{layout}:}{Currently unused.  Meant to store the layout of the plots on a device in the future.}
#' 
#'    \item{\code{mainTitle}:}{Currently unused.}
#' 
#'    \item{\code{panelLayout}:}{layout (rows and columns) of panels of each plot.  Can be a length 2 
#'         numeric vector or a length 0 vector, in which case it is not used.}
#' 
#'    \item{\code{maxPanels}:}{Maximum number of panels to display on each page. Overrides layout if of length > 0.}
#'  }
#' 
#' @section Methods:
#'  \describe{
#'    \item{show}{\code{signature(object = "multiTrellis")}: This print method plots the object}
#' 	}
#' 
#' @name multiTrellis-class
#' @rdname multiTrellis-class
#' @docType class
#' @exportClass multiTrellis
#' @author Mango Solutions
#' @examples 
#' \dontrun{
#' showClass("multiTrellis")
#' }
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

#' \code{panelLayout<-} assigns a layout to the panels of the plot held in the object \code{x}.
#' @title Assign panel layout
#' @param x \code{panelLayout<-} assigns a layout to the panels of the plot held in the object \code{x}
#' @param value A length 2 vector (number of rows and columns in the layout)
#' @return Updates the object "in place"
#' @author Mango Solutions
#' @keywords hplot
#' @examples 
#' \dontrun{  
#' x <- nmScatterPlot(mtcars, "mpg", "wt", bVars = "gear,carb")
#' panelLayout(x) <- c(2,2)
#' print(x)
#' }

"panelLayout<-" <-function(x, value)
{
	x@panelLayout <- value
	x
}