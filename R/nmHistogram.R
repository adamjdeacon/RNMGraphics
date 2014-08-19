
#' creates histograms of one or more NONMEM variables
#' @title NONMEM data histogram
#' @param obj The object from which data will be plotted (NMRun, NMProblem or data.frame)
#' @param vars Variables from which to generate a histogram (character vector or comma seperate string of names)
#' @param bVars "Trellis" variables on which to split data.  
#' @param iVar Subject identifier variable
#' @param refLine Controls addition of a reference line to the histogram(s).  Choices are "none", "mean" or "median".
#' @param type Determines the style of y-axis that is used for the plot (percentages, frequencies, or proportions)
#' @param addDensity Logical flag.  Should a density estimate be plotted? Only relevant for type = "density"
#' @param titles Plot title
#' @param xLabs x axis label
#' @param xRotAngle Angle by which to rotate the x-axis tick marks
#' @param extraSubset Currently unused
#' @param addGrid Currently unused
#' @param nint Number of intervals for the creation of X axis bars.  It functions identically to the nint parameter of the histogram function from the lattice package
#' @param breaks Control the calculation of breakpoints for the histogram.  It functions identically to the breaks parameter of the histogram function from the lattice package.
#' @param layout A length 2 vector which is passed in as the layout parameter to xyplot
#' @param maxPanels Maximum number of panels that should appear on each page of a graph.
#' @param maxTLevels f a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned.
#'        If a character vector or a vector of length greater than one, the explicit breakpoints.
#' @param problemNum Number of the problem (applicable to NMRun class only)
#' @param subProblems Number of the simulation subproblems to use (applicable to the NMSim* classes obly)
#' @param yAxisScaleRelations Y-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}. 
#' @param xAxisScaleRelations X-axis scale relations when panels are displayed. One of \code{"same"}, \code{"free"} or \code{"sliced"}. 
#' @param ... Additional parameters passed to histogram
#' @examples
#' \dontrun{ 
#'  Theoph.df <- as.data.frame(Theoph)
#'  nmHistogram(Theoph.df, vars = "conc", 
#'    title = "Theophiline concentration histogram", type = "density") 
#' }
#' @return An object of class multiTrellis
#' @author Mango Solutions
#' @keywords hplot
#' @exportMethod nmHistogram

nmHistogram <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
				 addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, xRotAngle = 0,
				 maxTLevels = Inf,  problemNum = 1, subProblems = 1, 
                 xAxisScaleRelations = c("same", "free", "sliced"), 
                 yAxisScaleRelations = c("same", "free", "sliced"), ...)
{
	RNMGraphicsStop("Not implemented for this class at the moment")
}

nmHistogram.NMRun <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
		addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, xRotAngle = 0,
		maxTLevels = Inf,  problemNum = 1, subProblems = 1, 
        xAxisScaleRelations = c("same", "free", "sliced"), 
        yAxisScaleRelations = c("same", "free", "sliced"), ...)
{

	prob <- getProblem(obj, problemNum)
	x <- as.list(match.call())
	x$obj <- prob
	do.call(nmHistogram, x[-1])
}

# TODO: handle simulated data

nmHistogram.NMProblem <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
							addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, xRotAngle = 0,
							maxTLevels = Inf,  problemNum = 1, subProblems = 1, 
                            xAxisScaleRelations = c("same", "free", "sliced"), 
                            yAxisScaleRelations = c("same", "free", "sliced"),
                            ...)
{
	dataSet <- nmData(obj, subProblemNum = subProblems )
	
	x <- as.list(match.call())
	x$obj <- dataSet
	
	do.call(nmHistogram, x[-1])
	
}

nmHistogram.data.frame <- function(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, 
							addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, xRotAngle = 0,
							maxTLevels = Inf,  problemNum = 1, subProblems = 1, 
                            xAxisScaleRelations = c("same", "free", "sliced"), 
                            yAxisScaleRelations = c("same", "free", "sliced"),
                            ...)
{
	## include removeEmpty option to prevent empty string errors
	## include error handling if not enough variables are provided
	vars <- CSLtoVector(vars, removeEmpty = TRUE)
	RNMGraphicsStopifnot(length(vars) > 0, "At least one variable must be provided to create this plot.\n")
	
	if(!(is.element(refLine, c("none", "mean", "median"))))
    {
		RNMGraphicsStop("Reference line parameter not valid!")
    }
	
    if(!(is.element(type, c("count", "percent", "density"))))
    {
		RNMGraphicsStop("Type parameter not valid!")
    }
	
    # the density line / curve will only be added if the y-axis is also of density
    # type
    
    if(type != "density")
    {
		# addDensity : logical flag indicating whether or not a density line should be added
        addDensity <- FALSE
    }
    
    # layout is determined by the maxPanels parameter if it is 
    # nonzero
    
	if(length(maxPanels) > 0)
    {
        layout <- NULL
    }
    
    # ensure that maxPanels is numeric, even if empty
	
    else maxPanels <- numeric(0)
		
    # the formula passed to histogram will be built by collapsing the vars vector
    # e.g. if vars = c("WRES", "IWRES"), the formula will be
    # ~ WRES + IWRES
    
	plotFormulas <- paste(" ~ ", paste(CSLtoVector(vars), collapse = "+") )
	if(missing(xLabs)) 
    {  
        xLabs <- paste(CSLtoVector(vars), collapse = "+")
    }
    
    # dataSet is the final data.frame to plot
    
	dataSet <- applyGraphSubset(obj)
	
    # if there is a set of by variables, process them
	if(!is.null(bVars))
	{
		bVars <- CSLtoVector(bVars)
		temp <- processTrellis(dataSet, bVars, maxLevels = maxTLevels, exemptColumns = iVar)
		dataSet <- coerceToFactors(temp$data, temp$columns)
		bVars <- temp$columns
		plotFormulas <- paste(plotFormulas, paste(bVars, collapse = "*"), sep = "|")
	}
	
    # set scales as necessary

	scales <- list(x = list(rot = xRotAngle), y = list())
	scales$y$relation <- match.arg(yAxisScaleRelations)
	scales$x$relation <- match.arg(xAxisScaleRelations)
	
    # extract the currently configured strip function
    stripfn <- getStripFun()

	# obtain a full set of graphical parameters
    graphParams <- getAllGraphParams()
	
	# Create main plot using "histogram" lattice function
	exp <- quote(histogram(as.formula(plotFormulas), main = titles, data = dataSet, xlab = xLabs, 
					par.settings = list(plot.polygon = graphParams$histogram, par.xlab.text = graphParams$"axis.text", 
					par.ylab.text = graphParams$"axis.text", par.main.text = graphParams$title.text,
					strip.background = graphParams$"strip.bg"), 
					refLine = refLine, type = type, addDensity = addDensity, panel = panel.nmHistogram, outer = TRUE, 
					strip = stripfn, nint = nint, graphParams = graphParams,
                    scales = scales,
                    ...))
	
	# Use break points if specified
	if(!missing(breaks)) exp$breaks <- breaks	

	# If X axis relation is set to "free" or "sliced" then set breaks to NULL (Mantis ticket 5130)
	if (scales$x$relation %in% c("free", "sliced")) {
		exp <- quote(histogram(as.formula(plotFormulas), main = titles, data = dataSet, xlab = xLabs, 
						par.settings = list(plot.polygon = graphParams$histogram, par.xlab.text = graphParams$"axis.text", 
								par.ylab.text = graphParams$"axis.text", par.main.text = graphParams$title.text,
								strip.background = graphParams$"strip.bg"), 
						refLine = refLine, type = type, addDensity = addDensity, panel = panel.nmHistogram, outer = TRUE, 
						strip = stripfn, nint = nint, graphParams = graphParams,
						scales = scales,breaks = NULL,
						...))
	}

	plt <- eval(exp)
	multiTrellis(list(plt), maxPanels = maxPanels)
}

#' @details panel.nmHistogram is a custom panel function for nmHistogram. In essence it just adds a density line and a reference line if needed.
#' @name panel functions
#' @aliases panel.nmBoxPlot panel.nmHistogram panel.overlaidScatter prepanel.nmBoxPlot
#' @param x Basic parameter passed straight to panel.histogram
#' @param refLine "none", "mean", or "median" - where to add a reference line
#' @param addDensity logical flag.  If TRUE, density estimate curve will be generated
#' @param graphParams Full set (list) of RNMGraphics graphical parameters 
#' @param ... 


panel.nmHistogram <- function(x, refLine, addDensity, graphParams, ...)
{
	# refVal will hold the value at which a reference line will be added 
    
    refVal <- switch(refLine,
			"none"   = NULL,
			"mean"   = mean(x, na.rm = TRUE),
			"median" = median(x, na.rm = TRUE))
	panel.histogram(x, ...)
	
    if(addDensity) 
    {
		panel.densityplot(x, col = graphParams$histogram$dens.col, lty = graphParams$histogram$dens.lty, lwd = graphParams$histogram$dens.lwd, ...)
    }
    reflineOpts <- graphParams$"refline"
	panel.abline(v = refVal, col = reflineOpts$col, lwd = reflineOpts$lwd, 
			lty = reflineOpts$lty, ...)

	
}

setGeneric("nmHistogram")
setMethod("nmHistogram", signature(obj = "NMProblem"), nmHistogram.NMProblem)
setMethod("nmHistogram", signature(obj = "NMRun"), nmHistogram.NMRun)
setMethod("nmHistogram", signature(obj = "data.frame"), nmHistogram.data.frame)
