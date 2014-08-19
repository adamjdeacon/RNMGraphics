

.mirrorPlot <- function(originalData, simulatedData, 
		xVars, yVars, gVar = NULL, bVars = NULL, iVar  = "ID", type = "p", addInterval = FALSE, intervalAlpha = 0.95, simNumber = 1, 
		... )
{
	
	assertClass(originalData, "data.frame")
	assertClass(simulatedData, "data.frame")
	
	# TODO: add column assertion logic
	
	if(!("NSIM" %in% colnames(simulatedData)))
	{
		RNMGraphicsWarning("Simulated data contains no NSIM column, so all of it will be plotted")
		simDataToPlot <- simulatedData
		simDataToPlot$NSIM <- 1
	}
	else
	{
		RNMGraphicsStopifnot(simNumber %in% simulatedData$NSIM, "Invalid simulation number selected", 
				match.call())
		simDataToPlot <- subset(simulatedData, NSIM == simNumber)
	}
	# make the original data have "SIM 0"
	
	originalData$NSIM <- "Original Data"
	
	# check which columns are in simulated and original data
	
	commonColumns <- intersect(colnames(originalData), colnames(simDataToPlot) )
	simDataToPlot$NSIM <- paste("Simulated Data:", simNumber, sep = "")
	# rearrange data so that the columns occur in the correct order prior to binding
#	originalDataReOrdered <- originalData[commonColumns]
#	ordering <- match(commonColumns, names(originalDataReOrdered))
#	originalDataReOrdered <- originalDataReOrdered[ordering]
#	
#	simDataToPlotReOrdered <- simDataToPlot[commonColumns]
#	ordering <- match(commonColumns, names(simDataToPlotReOrdered))
#	
#	simDataToPlotReOrdered <- simDataToPlotReOrdered[ordering]
#	
#	
	consolidatedData <- rbind( originalData[commonColumns], simDataToPlot[commonColumns]) 
	
	nmScatterPlot( consolidatedData, xVars = xVars, yVars = yVars, gVars = gVar, iVars = iVar, 
			bVars = c("NSIM", bVars) , types = type, ... )
}

mirrorPlot <- function(obj, xVars, yVars, gVar = NULL, bVars = NULL, iVar  = "ID", 
		type = "p", addInterval = FALSE, intervalAlpha = 0.95, simNumber = 1, 
		originalProblem = 1, simProblem = 2, ... )
{
	RNMGraphicsStop("mirrorPlot not implemented for this class\n", match.fun())	
}

setGeneric("mirrorPlot")

mirrorPlot.NMRun <- function(obj, xVars, yVars, gVar = NULL, bVars = NULL, iVar  = "ID", 
		type = "p", addInterval = FALSE, intervalAlpha = 0.95, simNumber = 1, 
		originalProblemNum = 1, simProblemNum = 2, ... )
{
	origProblem <- getProblem(obj, problemNumber = originalProblemNum)
	simProblem <- getProblem(obj, problemNumber = simProblemNum)
	
	
}
	