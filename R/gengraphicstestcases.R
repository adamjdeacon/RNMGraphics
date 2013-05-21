# SVN revision: $Rev$
# Date of last change: $LastChangedDate$
# Last changed by: $LastChangedBy$
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Generates a set of expected graphs to be used for testing.  These expected graphs are returned as a list of graphs, 
#' with one set for each different graph type (they are also duplicated for a different style).  
#' They may optionally be written to disk for comparison. 
#' @param outputDir Directory into which various files (such as the images) should be written
#' @param writeImages Logical flag.  Write the generated graphs to files?
#' @param writeRData Logical flag.  Write the generated graphs to .RData objects?
#' @param writeComparisonTable Logical flag.  Produce a table which can be used with the image comparison tool?
#' @param loadPack Load the RNMGraphics package when running?
#' @param resultDir String holding the directory where the results should go.  Used when producing comparison table. 
#' @param expectedDir Directory of expected graphs.  Used when producing comparison table.
#' @param comparisonTable Name of the comparison table file.
#' @title Generated expected graphs
#' @nord
#' @return A list with elements for each plot type, with default and non-standard stylings.
#' @author fgochez

genExpectedGraphs <- function(outputDir = "./", writeImages = TRUE, 
		writeRData = FALSE, writeComparisonTable = FALSE, loadPack = TRUE, 
		resultDir = outputDir,	expectedDir = file.path("..", "expected/"),
		comparisonTable = "testcases.csv")
{
	
	.dataList <- list()
	
	.dataList[[1]] <- data.frame(X = 1:10, Y = 1:10, Z = c(1:5, seq(from=6,to=7,length.out = 5)), 
			W = 10:1, G = rep(LETTERS[1:2], 5), B = rep(letters[1:2], each = 5))
	.dataList[[2]] <- data.frame(X = 1:20, Y = 1:20, Z = c(1:10, seq(from=11,to=12,length.out = 10)), 
			W = 20:1, G = rep(LETTERS[1:2], 10), B = rep(letters[1:2], each = 10))
	.dataList[[3]] <-  data.frame(X = 1:50, TIME = rep(1:25, 2), ID = rep(1:2, each = 25))
	
	.dataList[[4]] <- importNm(system.file(package = "RNMImport", "unittests/testdata/TestRun/TestData1.ctl"))
	.dataList[[5]] <- data.frame(X = rnorm(100), Y = 1)
	.dataList[[6]] <- data.frame(X = rep(seq(1, 2, length.out = 4), times = 4) + rep(c(0, 5, 10, 15), each = 4 ), Y = rep(1:4, times = 4),
			G = rep(letters[1:4], each = 4), H = rep(1:2, each = 8))
	
	# final data set for checking algorithm to force integer axes to only have integer tick marks
	.dataList[[7]] <- data.frame(Y = c(2, 3, 1, 3, 3, 1, 3, 2, 1, 3), 
								X = c(1.3520803, 0.6580209, -1.3252070, 0.6388983, 0.4507879,  
						0.5380433, 1.1361281, 0.3263466, 1.2457650, 0.9912256), 
				Z = c(1, 3, 1, 2, 4, 2, 4, 1, 2, 1))	
		
	.dataList[[8]] <- data.frame( X1 = seq(from = 1, to = 10, by = 0.5), X2 = seq(from = -4, to = 5, by = 0.5), 
			Y1 = seq(from = 10, to = 1, by = -0.5), Y2 = seq(from = 5, to = -4, by = -0.5) )
	
	if(loadPack)
	{
		require(RNMGraphics)
	}
	RNMGraphics:::initializeOptions()
	on.exit(RNMGraphics:::initializeOptions())
	
	if(Sys.info()["sysname"] == "Linux")
	{
		cat("Running under Linux\n")
		Sys.setenv("DISPLAY"=":5")
		setGraphParams("plot.symbol", list(alpha = 1))
		setGraphParams("plot.line", list(alpha = 1))
		setGraphParams("superpose.symbol", list(alpha = rep(1, 8)))
	}
	
	plots <- list("nmScatterPlot" = gen.nmScatterPlot(.dataList), "nmQQNorm" = gen.nmQQNorm(.dataList), 
			"nmBoxPlot" = gen.nmBoxPlot(.dataList), "nmScatterMatrix" = gen.nmScatterMatrix(.dataList), 
			"nmHistogram" = gen.nmHistogram(.dataList), "nmACPlot" = gen.nmACPlot(.dataList),
			"nmBarChart" = gen.nmBarChart(.dataList),
			"nmDotPlot" = gen.nmDotPlot(.dataList), "timeEventPlots" = gen.timeEventPlots(.dataList))
	
	
	setGraphParams("plot.symbol", list(alpha = 1, cex = 1.5, col = "green", pch = 2))
	setGraphParams("plot.line", list(alpha = 1, col = "red", lwd = 2, "lty" = 2))
	setGraphParams("superpose.symbol", list(cex = c(0.8, 1.3), col = c("black", "red"), pch = c(19,2)))
	
	plots2 <- list("nmScatterPlot" = gen.nmScatterPlot(.dataList), "nmQQNorm" = gen.nmQQNorm(.dataList), 
			"nmBoxPlot" = gen.nmBoxPlot(.dataList), "nmScatterMatrix" = gen.nmScatterMatrix(.dataList), 
			"nmHistogram" = gen.nmHistogram(.dataList), "nmACPlot" = gen.nmACPlot(.dataList),
			"nmBarChart" = gen.nmBarChart(.dataList), 
			"nmDotPlot" = gen.nmDotPlot(.dataList), "timeEventPlots" = gen.timeEventPlots(.dataList))
	# append to the names of the list elements
	names(plots2) <- paste(names(plots2), "_st", sep = "" )
	plots <- c(plots, plots2)
	
	for(n in names(plots))
	{
		if(writeRData)
		{
			stop("not working at the moment...")
			fName <- paste(names(plots)[i], ".RData", sep = "")
			print(plots[[i]])
			x <- recordPlot()
			save(x, file = file.path(outputDir, fName))
			dev.off()
			
		}
		if(writeImages)
		{
			for(i in seq_along(plots[[n]]))
			{
				if(Sys.info()["sysname"] == "Linux")
					png(file.path(outputDir, paste(n, i, ".png", sep = "")), width = 400, height = 400)
				else
					jpeg(file.path(outputDir, paste(n, i, ".jpg", sep = "")), width = 400, height = 400, quality = 75)
				print(plots[[n]][[i]])			
				dev.off()
			}
		}
	}
	if(writeComparisonTable)
	{
		plotNames <- lapply(seq_along(plots), function(i) paste(names(plots)[i], seq_along(plots[[i]]), sep = "") )
		plotNames <- do.call(c, plotNames)
		# browser()
		if(Sys.info()["sysname"] == "Linux")
			outfileNames <- paste(plotNames, ".png", sep = "")
		else
			outfileNames <- paste(plotNames, ".jpg", sep = "")
		outtable <- data.frame(filePath1 = resultDir, filePath2 = expectedDir,
				file1 = outfileNames, file2 = outfileNames, outFile = paste("_", outfileNames, sep = ""))
		write.csv(outtable, comparisonTable, row.names = FALSE)
	}
	RNMGraphics:::initializeOptions()
	
	invisible(plots)
}

gen.nmBarChart <- function(testData, secondaryGraphSettings = getAllGraphParams())
{
	plots <- vector(mode = "list", length = 2)
	CO2.df <- as.data.frame(CO2)
	plots[[1]] <- nmBarChart(CO2.df, "Plant", "Treatment", title = "CO2", xLab = "The Treatment", yLab = "The type")
	
	plots[[2]] <- nmBarChart(CO2.df, "Treatment", "Plant", bVar = "Type" )
	
	# tests xRotAngle, binning of x-axis (explicit cuts), by variable (issue 1156)
	
	plots[[3]] <- nmBarChart(testData[[1]], xBin = "0,3,7,10", xVar = "X", yVar = "G", bVar = "B", xRotAngle = 90  )
	
	# non-explicit cut, multiple by-vars (issue 1156)
	
	plots[[4]] <- nmBarChart(mtcars, xVar = "mpg", yVar = "carb", bVar = "vs,am", xBin = 2)
	
	# 3022 - commas should be allowed in axis labels
	
	plots[[5]] <- nmBarChart( CO2.df, "Treatment", "Plant", xLab = "a,b", yLab = "a,b" )
	
	# test styles

	oldGraphSettings <- getAllGraphParams()
	setAllGraphParams(secondaryGraphSettings)
	plots[[6]] <- nmBarChart(CO2.df, "Treatment", "Plant", bVars = "Treatment", xLab = "xlab", yLab = "ylab", title = "style test")
	setAllGraphParams(oldGraphSettings)
	
    # issue 3741
    
    plots[[7]] <- nmBarChart(CO2.df, "Treatment", "Plant", bVar = "Type", yAxisScaleRelations = "free" )
    
    plots[[8]] <- nmBarChart(CO2.df, "Treatment", "Plant", bVar = "Type", 
            xAxisScaleRelations = "free", layout = c(1,2) )    
    
	plots
    
    
}

gen.nmScatterPlot <- function(testDataList, secondaryGraphSettings = getAllGraphParams())
{
	plots <- vector(length = 9, mode = "list")
	testData <- testDataList[[1]]

	# test type = "t"
	test2 <- nmScatterPlot(testDataList[[1]], "X", "Y, W", iVar = "G", types = "t",
			addGrid = FALSE, titles = "BAR", pch = 19, xLab = "XLAB", yLab = "YLAB")
	
	plots[[1]] <- test2
	
	testData2 <- testDataList[[2]]
	
	# test simple use of by variable
	test3 <- nmScatterPlot(testData2, "X", "Y",bVar = "B")
	plots[[2]] <- test3
	
	# test very simple case
	test4 <- nmScatterPlot(testData2, "X", "Z")
	plots[[3]] <- test4
		# test equal axis scales
	plots[[4]] <-  nmScatterPlot(testData2, "X", "Z", equalAxisScales = TRUE)
	# test overlaid + bVar
	# TODO: might be worth trying other types
	
	plots[[5]] <- nmScatterPlot(subset(nmData(testDataList[[4]]), ID %in% 1:10), "TIME", "PRED, IPRED", 
			overlaid = TRUE, bVars = "SEX", addLegend = TRUE, type = "o") 
	# free y axis relations
	plots[[6]] <-  nmScatterPlot(testDataList[[1]], "X", "Y, W", bVar = "B", gVar = "G", type = "p", addLegend = TRUE, yAxisScaleRelations = "free")
	
	# test the different types
	
	plots[[7]] <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
				type = "l", addLegend = TRUE)
	plots[[8]] <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
				type = "p", addLegend = TRUE)
	plots[[9]] <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
				type = "t", addLegend = TRUE)
	plots[[10]] <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
				type = "i", addLegend = TRUE)
	plots[[11]] <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
				type = "o", addLegend = TRUE)
	
	# finally, check maxTLevels and maxPanels, along with generics and subset
	
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	
	plots[[12]] <- nmScatterPlot(testDataList[[4]], "DV", "PRED", maxTLevels = 2, maxPanels = 9, bVars = "ID,TIME")
	plots[[13]] <- nmScatterPlot(getProblem(testDataList[[4]],1), "DV", "PRED", maxTLevels = 2, maxPanels = 9, bVars = "ID,TIME")
	plots[[14]] <- nmScatterPlot(testDataList[[7]], "X", "Y", titles = "Y axis should be integer only")
	plots[[15]] <- nmScatterPlot(testDataList[[7]], "Y", "X", titles = "X axis should be integer only"
			, uniqueX = 2)
	plots[[16]] <- nmScatterPlot(testDataList[[7]], "X", "Y,Z", 
			titles = "Two y variables, behavior is ignored", yAxisScaleRelations = "free", )
	
	nmPlotData <- nmData(testDataList[[4]])
	
	nmPlotData <- addDerivedCategorical(nmPlotData, "TIME", "TIME.CUT")
	graphSubset(nmPlotData) <- "ID < 10"
	plots[[17]] <- nmScatterPlot(nmPlotData, "TIME.CUT", "PRED, IPRED",
			xRotAngle=90, bVar = "SEX", logY = TRUE, uniqueX = 2 )
	plots[[18]] <- nmScatterPlot(nmPlotData, "TIME.CUT", "PRED, IPRED", xRotAngle=90, bVar = "SEX", 
			logY = TRUE, overlaid = TRUE, uniqueX = 2 )
	
	# logX test needed
	plots[[19]] <- nmScatterPlot(nmPlotData, yVars = "DV", xVars =  "TIME", logX = TRUE, type = "l")
	# logX and logY
	plots[[20]] <- nmScatterPlot(nmPlotData, yVars = "DV", xVars = "TIME", logX = TRUE, logY = TRUE, type = "l")
	
	# clear check that the legend for overlaid variables is correct (mantis issue 1758)
	
	plots[[21]] <- nmScatterPlot(testDataList[[1]], "X", "Z, Y", overlaid = TRUE, addLegend = TRUE, addGrid = FALSE, title = "Legend for overlaid variables is correct")
	
	# clear check that the legend for overlaid variables is correct for type = "i" (mantis issue 1758, re-opened).
	
	plots[[22]] <- nmScatterPlot(testDataList[[2]], yVars = "Z,Y", xVars = "X", iVar = "G", type = "i", overlaid = TRUE, addLegend = TRUE)
	
	# box-plot when x-axis has sufficiently few unique values (issue 1143)
	
	plots[[23]] <- nmScatterPlot(testDataList[[6]], xVar = "G", yVar = "Y", bVar = "H",
			yAxisScaleRelations = "free", titles = "Scatter plot is box plot")
	
	# issue 2670 : overlaid box-plots disregarded
	
	plots[[24]] <- nmScatterPlot(testDataList[[1]], xVar = "G", yVar = "Y,Z", yAxisScaleRelations = "free",
				titles = "Scatter plot is box plot, overlaid y-axis disregarded", overlaid = TRUE)

	# issue 2044 : logging of negative numbers ignored

	logTestData <- testDataList[[1]]
	logTestData$Z[1:5] <- 1 -  logTestData$Z[1:5]
	
	# One y-axis var has negative values, other does not
	
	plots[[25]] <- nmScatterPlot(logTestData, xVar = "X", yVar = "Y,Z", yAxisScaleRelations = "free",
			logY = TRUE,
			titles = "2044: One y-axis var has negative values, other does not")
	
	# logged x-axis
	
	plots[[26]] <- nmScatterPlot(logTestData, xVar = "Z", yVar = "Y",
			logX = TRUE,titles = "2044: logged x-axis")
	
	# both axes logged

	plots[[27]] <- nmScatterPlot(logTestData, xVar = "X", yVar = "Z", logX = TRUE, logY = TRUE, 
			title = "2044: Both axes logged")
	
	##### 
	
	# issue 2755 - match axis scales with logging allowed
	
	x <- testDataList[[8]]

	plots[[28]] <- nmScatterPlot(x, xVar = "X1", yVar = "Y2", 
			logY = TRUE, equalAxisScales = TRUE)
	
	plots[[29]] <- nmScatterPlot(x, xVar = "X1", yVar = "Y2", 
			logX = TRUE, equalAxisScales = TRUE)
	
	plots[[30]] <- nmScatterPlot(x, xVar = "X1", yVar = "Y1,Y2", 
			logY = TRUE, equalAxisScales = TRUE, yAxisScaleRelations = "free")
	
	plots[[31]] <- nmScatterPlot(x, xVar = "X2", yVar = "Y1", 
			logX = TRUE, equalAxisScales = TRUE, logY = TRUE)
	
	plots[[32]] <- nmScatterPlot(x, xVar = "X2", yVar = "Y1,Y2", logX = TRUE, 
			logY = TRUE, equalAxisScales = TRUE)
	
	plots[[33]] <- nmScatterPlot(x, xVar = "X1", yVar = "Y1,Y2", logX = TRUE, 
			logY = TRUE, equalAxisScales = TRUE, overlaid = TRUE)
	
	# issue 3032 : commas should be allowed in axis labels

	plots[[34]] <- nmScatterPlot(testDataList[[1]], "X", "Y, W", xLab = "A,B", yLab = "A,B", overlaid = TRUE)
	
	# issue 2120 : Should now be possible to override global graphical styles via graphParams parameter

	plots[[35]] <- nmScatterPlot(testDataList[[1]], "X", "Y, W", iVar = "G", types = "t", yAxisScaleRelations = "free", 
		addGrid = TRUE, titles = "BAR", pch = 19, xLab = "XLAB", yLab = "YLAB", graphParams = secondaryGraphSettings )
	
	plots[[36]] <- nmScatterPlot(testDataList[[1]], "X", "Y, W", iVar = "G", types = "o", overlaid = TRUE, graphParams = secondaryGraphSettings, addLegend = TRUE )
	
	plots[[37]] <- nmScatterPlot(testDataList[[2]], "X", "Z", iVar = "G", gVar = "B", types = "p", addGrid = FALSE, graphParams = secondaryGraphSettings, addLegend = TRUE, idLine = TRUE, addLoess = TRUE )
	
	# issue 3439
	
	plots[[38]] <- nmScatterPlot(testDataList[[1]][1,], "X", "Y", uniqueX = 0, title = "Can generate a continuous plot with a single unique x-axis value")
	
    # issue 3741 : it should be possible to set x-axis relations as well

    plots[[39]] <- nmScatterPlot(testDataList[[1]], "X", "Y", bVar = "B", xAxisScaleRelations = "free")    
    
    plots[[40]] <- nmScatterPlot(testDataList[[1]], "X", "Y", bVar = "B", xAxisScaleRelations = "same")    
    
    # check that axis logging works with different x axis relations
    
    plots[[41]] <- nmScatterPlot(testDataList[[1]], "X", "Y", bVar = "B", xAxisScaleRelations = "free", logX = TRUE)    
    
    # check that x-axis and y-axis relations can be modified for overlaid plots 
    
    plots[[42]] <- nmScatterPlot(testDataList[[1]], xVar = "X", yVar = "Y,W", bVar = "B", overlaid = TRUE, yAxisScaleRelations = "free")
    plots[[43]] <- nmScatterPlot(testDataList[[1]], xVar = "X", yVar = "Y,W", bVar = "B", overlaid = TRUE, xAxisScaleRelations = "free",
            layout = c(1,2))
    
    # issue 3759 : add reference lines
    # check addition of reference lines
    testReflines <- list( c(0, 5), c(2, 0), 1, "a" )

    # non-overlaid plot

    plots[[44]] <- nmScatterPlot(testDataList[[1]], "X", "Y", addLegend = TRUE,
            title = "Overlaid lines work for non-overlaid scatterplot", abLines = testReflines)

    # overlaid plot

    plots[[45]] <- nmScatterPlot(testDataList[[1]], "X", "Y, Z", addLegend = TRUE, 
            title = "Overlaid lines work for overlaid scatterplot", overlaid = TRUE,
            abLines = testReflines, bVar = "G")
    
    # ignore bad abLines parameter (overlaid plot)
    
    plots[[46]] <- nmScatterPlot(testDataList[[1]], "X", "Y, Z", addLegend = TRUE, 
            title = "Bad overlaid lines ignored for overlaid scatterplot", overlaid = TRUE,
            abLines = c(1,0), bVar = "G")
    
    # ignore bad abLines parameter (non-overlaid plot)
    
    plots[[47]] <- nmScatterPlot(testDataList[[1]], "X", "Y, Z", addLegend = TRUE, 
        title = "Bad overlaid lines ignored for non-overlaid scatterplot", overlaid = FALSE,
        abLines = c(1,0), bVar = "G")

	# test loess line	
	oldLtySet <- getAllGraphParams()$superpose.line$lty
	setGraphParams("superpose.line", list(lty = rep(1:4, 2)))

	plots[[48]] <- nmScatterPlot(subset(nmData(testDataList[[4]]), ID %in% 1:10), "TIME", "PRED, IPRED", 
			overlaid = TRUE, bVars = "ID", addLoess = TRUE, addLegend = TRUE, type = "p") 
	setGraphParams("superpose.line", list(lty = oldLtySet))
    plots
}

gen.nmBoxPlot <- function(testDataList, secondaryGraphSettings = getAllGraphParams())
{
	testData <- testDataList[[1]]
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 3)
	
	# check labels / simple plot
	plots[[1]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", medianLines = FALSE)
	
	CO2.df <- as.data.frame(CO2)
	
	# free y axis relations, bVar
	
	plots[[2]] <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", bVars = "Treatment", 
			yAxisScaleRelations = "free", medianLines = FALSE)
	# continuous variable on x axis
	plots[[3]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", contVarOnX = TRUE, medianLines = FALSE)
		
	
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	
	# check maxTLevels and maxPanels, along with generics and subset and factBin, xRotAngle
	plots[[4]] <- nmBoxPlot(testDataList[[4]], contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "ID,TIME", maxTLevels = 2, maxPanels = 9, xRotAngle = 90, medianLines = FALSE, modifyOnResidual = FALSE)
	plots[[5]] <- nmBoxPlot(getProblem(testDataList[[4]],1), contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "ID,TIME", maxTLevels = 2, maxPanels = 9, xRotAngle = 90, medianLines = FALSE, modifyOnResidual = FALSE)
	
	# test balanced axes, horizontal lines, median lines
	# issue 2310
	plots[[6]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", 
			medianLines = TRUE, balancedContAxis = TRUE, hLines = c(-1,0,2))
	
	# same as before, but with continuous variable on x-axis

	plots[[7]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
		xLabs = "Group", yLabs = "Extra sleep", contVarOnX = TRUE,  
		medianLines = TRUE, balancedContAxis = TRUE)
	
	# customized residual plot, should force balanced continuous axes and reference line at x = 0
	# issue 2310
	
	plots[[8]] <- nmBoxPlot(testDataList[[4]], contVar = "WRES", factVar = "PRED", factBin = 3,
		bVar = "TIME", maxTLevels = 2, xRotAngle = 90, modifyOnResidual = TRUE, layout = c(3, 1))

	# same as above, different residual variables allowed
	# issue 2310

	plots[[9]] <- nmBoxPlot(testDataList[[4]], contVar = "RES", factVar = "SEX",modifyOnResidual = TRUE, 
			residVars = "RES")
	
	# issue 3032 - commas should be allowed in axis labels

	plots[[10]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
		xLabs = "a,b", yLabs = "a,b", medianLines = FALSE)
	# 
	oldGraphSettings <- getAllGraphParams()
	
	# test styles
	
	setAllGraphParams(secondaryGraphSettings)
	plots[[11]] <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", bVars = "Treatment", 	yAxisScaleRelations = "free", medianLines = FALSE, hLines = 400)
	setAllGraphParams(oldGraphSettings)
	
    # issue 3741

    plots[[12]] <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", 
            bVars = "Treatment", contVarOnX = TRUE,  xAxisScaleRelations = "free", medianLines = FALSE, hLines = 400)    
    
	plots	
}

gen.nmHistogram <- function(testDataList, secondaryGraphSettings = getAllGraphParams())
{
	testData <- testDataList[[1]]
	
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 4)
	test1 <- nmHistogram(testData, vars = "X, Z", bVar = "B", xLabs = "HELLO", titles = "TITLE")
	plots[[1]] <- test1
	
	test2 <- nmHistogram(testData, vars = "X, Z", bVar = "B", refLine = "median")
	plots[[2]] <- test2
	
	test3 <- nmHistogram(testData, vars = "Z", refLine = "mean")
	plots[[3]] <- test3
	
	test4 <- nmHistogram(testData, vars = "X, Z", bVar = "B", refLine = "median", addDensity = TRUE, type = "density")
	plots[[4]] <- test4
	
	# test styles
	
	oldGraphSettings <- getAllGraphParams()
	setAllGraphParams(secondaryGraphSettings)
	plots[[5]] <- nmHistogram(testData, vars = "X, Z", bVar = "B", xLabs = "HELLO", titles = "TITLE", addDensity = TRUE, type = "density")
	setAllGraphParams(oldGraphSettings)
	
	# check maxTLevels and maxPanels, along with generics and subset
	testData3 <- testDataList[[4]]
	graphSubset(testData3) <- "ID < 10"
	plots[[6]] <- nmHistogram(testData3, "WRES, IWRES", bVar = "AGE", maxTLevels = 3, maxPanels = 4)
	
	# check breaks 
	
	plots[[7]] <- nmHistogram(testData, "Z", breaks = c(1, 5, 7))
	
    # test x and y axis scale adjustment (issue 3741)

    plots[[8]] <- nmHistogram(testData, vars = "X, Z", 
            bVar = "B", xLabs = "HELLO", titles = "TITLE" ,
            xAxisScaleRelations = "free")
    plots[[9]] <- nmHistogram(testData, vars = "X, Z", 
            bVar = "B", xLabs = "HELLO", titles = "TITLE", 
            yAxisScaleRelations = "free")
    plots[[10]] <- nmHistogram(testData, vars = "X, Z", 
            bVar = "B", xLabs = "HELLO", titles = "TITLE", 
            yAxisScaleRelations = "free", xAxisScaleRelations = "free")
	plots
}


gen.nmScatterMatrix <- function(testDataList, secondaryGraphSettings = getAllGraphParams())
{
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 2)
	
	plots[[1]] <- nmScatterMatrix(testData2, "X,Y", bVars = "B")
	# check the try/catch loess failure logic
	plots[[2]] <- nmScatterMatrix(testData2, "X,Y", addLoess = TRUE, title = "Foo")
	
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	plots[[3]] <- nmScatterMatrix(testDataList[[4]], 
			"DV,PRED,IPRED","TIME,ID", iVar = "SUBJ", maxTLevels = 4, maxPanel = 9)
	
	plots[[4]] <- nmScatterMatrix(getProblem(testDataList[[4]]), 
			"DV,PRED,IPRED","TIME,ID", iVar = "SUBJ", maxTLevels = 4, maxPanel = 9)
	
	# styling
	
	oldGraphSettings <- getAllGraphParams()
	
	# test styles
	
	setAllGraphParams(secondaryGraphSettings)
	plots[[5]] <- nmScatterMatrix(testDataList[[4]], "DV,PRED", addLoess = TRUE, title = "Foo", bVar = "SEX")
	setAllGraphParams(oldGraphSettings)
	plots
}

gen.nmQQNorm <- function(testData, secondaryGraphSettings = getAllGraphParams())
{
	plots <- vector(mode = "list", length = 3)
	test1 <- nmQQNorm(sleep, "extra", bVar = "group", xLabs = "Normal distribution", titles = "Sleep data", yLabs = "Extra sleep")
	plots[[1]] <- test1
	
	
	plots[[2]]<- nmQQNorm(sleep, "extra")
	plots[[3]] <- nmQQNorm(sleep, "extra", qqLine = FALSE)
	
	
	#####okimberlin: tests the scale argument
	
	plots[[4]]<- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="free")
	plots[[5]]<- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="sliced")
	plots[[6]]<- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="same")
	
	# test generic, etc 
	graphSubset(testData[[4]]@problems[[1]]) <- "ID < 10"
	plots[[7]] <- nmQQNorm(testData[[4]], "IWRES", bVars = "ID,TIME",
			maxTLevels = 2,maxPanels = 9 )
	plots[[8]] <- nmQQNorm(getProblem(testData[[4]]), "IWRES", bVars = "ID,TIME",
			maxTLevels = 2,maxPanels = 9 )
	
	# test styling
	
	oldSettings <- getAllGraphParams()
	setAllGraphParams(secondaryGraphSettings)
	plots[[9]] <- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="free", title = "Swiss", yLab = "Fert and Mort", xLab = "Norm")
	setAllGraphParams(oldSettings)
	
    # (issue 3741) test xAxisScaleRelations
    ChickWeight.df <- as.data.frame(ChickWeight)
    plots[[10]]<- nmQQNorm(ChickWeight.df,"weight", bVar =  "Diet", xAxisScaleRelations="free")
    
	plots
}

# minimal test, other features should be tested by nmScatterPlot tests

gen.nmACPlot <- function(testDataList, secondaryGraphSettings = getAllGraphParams())
{
	testData3 <- testDataList[[3]]
	plots <- list()
	plots[[1]]<- nmACPlot(testData3, "X")
	
	# test styling
	
	oldSettings <- getAllGraphParams()
	setAllGraphParams(secondaryGraphSettings)
	
	testData <- cbind(testData3, G = rep(c("A","B"), times = 25))
	plots[[2]] <- nmACPlot(testData, "X", timeVar = "TIME", iVar = "ID", gVar = "G", xLab = "-X-", yLab = "X lagged",
			title = "Style test", addLegend = TRUE)
	plots[[3]] <- nmACPlot(testData, "X", timeVar = "TIME", iVar = "ID", bVar = "G", xLab = "-X-", yLab = "X lagged",
			title = "Style test 2")
	setAllGraphParams(oldSettings)
	
	plots
}

gen.nmDotPlot <- function(testData, secondaryGraphSettings = getAllGraphParams())
{
	plots <- list()
	plots[[1]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W")
	plots[[2]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", bVars = "G", title = "Test 2",
			 xLab = "xlabel", yLab = "ylabel")
	plots[[3]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X", gVar = "G", title = "Test 3",
			 xLab = "xlabel", yLab = "ylabel")
	plots[[4]] <- nmDotPlot(testData[[1]], factVar = "Y", contVar = "X", title = "Test 4",
			 xLab = "xlabel", yLab = "ylabel", maxFactPerPanel = 5)
	
	graphSubset(testData[[4]]@problems[[1]]) <- "ID < 10"
	
	# check maxFactPerPanel, generics, etc
	
	plots[[5]] <- nmDotPlot(testData[[4]], factVar = "ID", contVar = "WT", bVars = "TIME", 
			maxTLevels = 4, title = "Test 5", gVar = "SEX", addLegend = TRUE )
	
	plots[[6]] <- nmDotPlot(getProblem(testData[[4]]), factVar = "ID", contVar = "WT", bVars = "TIME", 
			maxTLevels = 4, title = "Test 5", gVar = "SEX", addLegend = TRUE )
	
	# Tests that the maxFactPerPanel algorithm has now been corrected.
	
	testDf <- data.frame(X = rep(1:2, each = 10), ID = rep(1:10, length.out = 20))
	
	plots[[7]] <- nmDotPlot(testDf, factVar = "ID", contVar = "X", maxFactPerPanel = 4)
	
	# tests fix for issue 3032 - commas should be allowed on axes
	
	plots[[8]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W",
							yLab = "a,b", xLab = "a,b")
	
	# tests fix for issue 3098 : auxilliary trellis variable created for "maxFactPerPanel" should never be binned 
	
	testDf2 <- data.frame(X = rep(1, 100), ID = 1:100)
	
	plots[[9]] <- nmDotPlot(testDf2, contVar = "X", factVar = "ID", title = "ID.GRP is not binned", maxFactPerPanel = 5,
			maxTLevels = 8)

	# test styles

	oldSettings <- getAllGraphParams()
	setAllGraphParams(secondaryGraphSettings)
	plots[[10]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", bVars = "G", title = "Test 2",
			xLab = "xlabel", yLab = "ylabel")
	setAllGraphParams(oldSettings)

	# issue 3741 : adding scale setting option
    
    plots[[11]]  <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", yAxisScaleRelations = "free") 
    plots[[12]]  <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", xAxisScaleRelations = "free")
    plots[[13]]  <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", 
            xAxisScaleRelations = "free", yAxisScaleRelations = "free")
    
	plots
	
}

gen.timeEventPlots <- function(testData, secondaryGraphSettings = getAllGraphParams())
{
	x <- nmData(testData[[4]])
	
	y <- subset(x, ID %in% 2:4)
	plots <- list()
	plots[[1]] <- timeEventSPlot(x, title = "Time/event", xLab = "Time", yLab = "Concentration",
			subjectNum = 2:4)
	plots[[2]] <- timeEventDPlot(y, title = "Time/event", xLab = "Time", yLab = "Subject")
	
	# test styles
	
	oldSettings <- getAllGraphParams()
	setAllGraphParams(secondaryGraphSettings)
	
	plots[[3]] <- timeEventSPlot(y, title = "Time/event", xLab = "Time", yLab = "Concentration", layout = c(3,1))
	plots[[4]] <- timeEventDPlot(y, title = "Time/event", xLab = "Time", yLab = "Subject")
	
	setAllGraphParams(oldSettings)
    
    plots[[5]] <- timeEventSPlot(y, title = "Time/event", xLab = "Time", yLab = "Concentration", layout = c(3,1),
             yAxisScaleRelations = "free")
    
    plots[[6]] <- timeEventSPlot(y, title = "Time/event", xLab = "Time", yLab = "Concentration", layout = c(1,3),
            expX = TRUE, xRotAngle = 45)
    
    plots[[7]] <- timeEventSPlot(y, expY = TRUE, xAxisScaleRelations = "free")
    
    Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0))))
    plots[[8]] <- timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc", doseVar = "Dose", evtVar = "Evt", iVar = "Subject")
    
	plots
}
