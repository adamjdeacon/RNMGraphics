
# test type = "t"
test01 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	plot1 <- nmScatterPlot(testDataList[[1]], "X", "Y, W", iVar = "G", types = "t",
			addGrid = FALSE, titles = "BAR", pch = 19, xLab = "XLAB", yLab = "YLAB")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# test simple use of by variable
test02 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData2 <- testDataList[[2]]
	
	plot2 <- nmScatterPlot(testData2, "X", "Y",bVar = "B")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# test very simple case
test03 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData2 <- testDataList[[2]]
	
	plot3 <- nmScatterPlot(testData2, "X", "Z")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test equal axis scales
test04 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData2 <- testDataList[[2]]
	plot4 <-  nmScatterPlot(testData2, "X", "Z", equalAxisScales = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test overlaid + bVar
# TODO: might be worth trying other types
test05 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot5 <- nmScatterPlot(subset(nmData(testDataList[[4]]), ID %in% 1:10), "TIME", "PRED, IPRED", 
			overlaid = TRUE, bVars = "SEX", addLegend = TRUE, type = "o") 
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# free y axis relations
test06 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot6 <-  nmScatterPlot(testDataList[[1]], "X", "Y, W", bVar = "B", gVar = "G", type = "p", addLegend = TRUE, yAxisScaleRelations = "free")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test the different types
test07 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot7 <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
			type = "l", addLegend = TRUE)
	plot8 <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
			type = "p", addLegend = TRUE)
	plot9 <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
			type = "t", addLegend = TRUE)
	plot10 <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
			type = "i", addLegend = TRUE)
	plot11 <- nmScatterPlot(testDataList[[6]], "X", "Y", iVar = "G", gVar = "H",
			type = "o", addLegend = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot9))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot9.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot10))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot10.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot11))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot11.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# finally, check maxTLevels and maxPanels, along with generics and subset
test08 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	
	plot12 <- nmScatterPlot(testDataList[[4]], "DV", "PRED", maxTLevels = 2, maxPanels = 9, bVars = "ID,TIME")
	plot13 <- nmScatterPlot(getProblem(testDataList[[4]],1), "DV", "PRED", maxTLevels = 2, maxPanels = 9, bVars = "ID,TIME")
	plot14 <- nmScatterPlot(testDataList[[7]], "X", "Y", titles = "Y axis should be integer only")
	plot15 <- nmScatterPlot(testDataList[[7]], "Y", "X", titles = "X axis should be integer only", uniqueX = 2)
	plot16 <- nmScatterPlot(testDataList[[7]], "X", "Y,Z", 
			titles = "Two y variables, behavior is ignored", yAxisScaleRelations = "free", )
	
	nmPlotData <- nmData(testDataList[[4]])
	
	nmPlotData <- addDerivedCategorical(nmPlotData, "TIME", "TIME.CUT")
	graphSubset(nmPlotData) <- "ID < 10"
	plot17 <- nmScatterPlot(nmPlotData, "TIME.CUT", "PRED, IPRED",
			xRotAngle=90, bVar = "SEX", logY = TRUE, uniqueX = 2 )
	plot18 <- nmScatterPlot(nmPlotData, "TIME.CUT", "PRED, IPRED", xRotAngle=90, bVar = "SEX", 
			logY = TRUE, overlaid = TRUE, uniqueX = 2 )
	# logX
	plot19 <- nmScatterPlot(nmPlotData, yVars = "DV", xVars =  "TIME", logX = TRUE, type = "l")
	# logX and logY
	plot20 <- nmScatterPlot(nmPlotData, yVars = "DV", xVars = "TIME", logX = TRUE, logY = TRUE, type = "l")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot12))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot12.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot13))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot13.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot14))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot14.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot15))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot15.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot16))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot16.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot17))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot17.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot18))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot18.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot19))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot19.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot20))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot20.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# clear check that the legend for overlaid variables is correct (mantis issue 1758)
test09 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot21 <- nmScatterPlot(testDataList[[1]], "X", "Z, Y", overlaid = TRUE, addLegend = TRUE, addGrid = FALSE, title = "Legend for overlaid variables is correct")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot21))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot21.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# clear check that the legend for overlaid variables is correct for type = "i" (mantis issue 1758, re-opened).
test10 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot22 <- nmScatterPlot(testDataList[[2]], yVars = "Z,Y", xVars = "X", iVar = "G", type = "i", overlaid = TRUE, addLegend = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot22))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot22.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# box-plot when x-axis has sufficiently few unique values (issue 1143)
test11 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot23 <- nmScatterPlot(testDataList[[6]], xVar = "G", yVar = "Y", bVar = "H",
			yAxisScaleRelations = "free", titles = "Scatter plot is box plot")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot23))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot23.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 2670 : overlaid box-plots disregarded
test12 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList

	plot24 <- nmScatterPlot(testDataList[[1]], xVar = "G", yVar = "Y,Z", yAxisScaleRelations = "free",
			titles = "Scatter plot is box plot, overlaid y-axis disregarded", overlaid = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot24))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot24.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 2044 : logging of negative numbers ignored
test13 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList

	logTestData <- testDataList[[1]]
	logTestData$Z[1:5] <- 1 -  logTestData$Z[1:5]
	
	# One y-axis var has negative values, other does not
	plot25 <- nmScatterPlot(logTestData, xVar = "X", yVar = "Y,Z", yAxisScaleRelations = "free",
			logY = TRUE,
			titles = "2044: One y-axis var has negative values, other does not")
	# logged x-axis
	plot26 <- nmScatterPlot(logTestData, xVar = "Z", yVar = "Y",
			logX = TRUE,titles = "2044: logged x-axis")
	# both axes logged
	plot27 <- nmScatterPlot(logTestData, xVar = "X", yVar = "Z", logX = TRUE, logY = TRUE, 
			title = "2044: Both axes logged")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot25))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot25.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot26))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot26.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot27))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot27.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 2755 - match axis scales with logging allowed
test14 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	x <- testDataList[[8]]
	plot28 <- nmScatterPlot(x, xVar = "X1", yVar = "Y2", 
			logY = TRUE, equalAxisScales = TRUE)
	plot29 <- nmScatterPlot(x, xVar = "X1", yVar = "Y2", 
			logX = TRUE, equalAxisScales = TRUE)
	plot30 <- nmScatterPlot(x, xVar = "X1", yVar = "Y1,Y2", 
			logY = TRUE, equalAxisScales = TRUE, yAxisScaleRelations = "free")
	plot31 <- nmScatterPlot(x, xVar = "X2", yVar = "Y1", 
			logX = TRUE, equalAxisScales = TRUE, logY = TRUE)
	plot32 <- nmScatterPlot(x, xVar = "X2", yVar = "Y1,Y2", logX = TRUE, 
			logY = TRUE, equalAxisScales = TRUE)
	plot33 <- nmScatterPlot(x, xVar = "X1", yVar = "Y1,Y2", logX = TRUE, 
			logY = TRUE, equalAxisScales = TRUE, overlaid = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot28))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot28.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot29))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot29.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot30))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot30.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot31))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot31.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot32))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot32.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot33))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot33.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3032 : commas should be allowed in axis labels
test15 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot34 <- nmScatterPlot(testDataList[[1]], "X", "Y, W", xLab = "A,B", yLab = "A,B", overlaid = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot34))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot34.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 2120 : Should now be possible to override global graphical styles via graphParams parameter
test16 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	plot35 <- nmScatterPlot(testDataList[[1]], "X", "Y, W", iVar = "G", types = "t", yAxisScaleRelations = "free", 
			addGrid = TRUE, titles = "BAR", pch = 19, xLab = "XLAB", yLab = "YLAB", graphParams = .RNMGraphicsTestEnv$newStyles )
	plot36 <- nmScatterPlot(testDataList[[1]], "X", "Y, W", iVar = "G", types = "o", overlaid = TRUE, 
			graphParams = .RNMGraphicsTestEnv$newStyles, addLegend = TRUE )
	plot37 <- nmScatterPlot(testDataList[[2]], "X", "Z", iVar = "G", gVar = "B", types = "p", addGrid = FALSE, 
			graphParams = .RNMGraphicsTestEnv$newStyles, addLegend = TRUE, idLine = TRUE, addLoess = TRUE )
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot35))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot35.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot36))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot36.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot37))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot37.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3439
test17 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	
	plot38 <- nmScatterPlot(testDataList[[1]][1,], "X", "Y", uniqueX = 0, title = "Can generate a continuous plot with a single unique x-axis value")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot38))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot38.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3741 : it should be possible to set x-axis relations as well
test18 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	plot39 <- nmScatterPlot(testDataList[[1]], "X", "Y", bVar = "B", xAxisScaleRelations = "free")    
	plot40 <- nmScatterPlot(testDataList[[1]], "X", "Y", bVar = "B", xAxisScaleRelations = "same")    
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot39))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot39.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot40))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot40.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check that axis logging works with different x axis relations
test19 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	plot41 <- nmScatterPlot(testDataList[[1]], "X", "Y", bVar = "B", xAxisScaleRelations = "free", logX = TRUE)    
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot41))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot41.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check that x-axis and y-axis relations can be modified for overlaid plots
test20 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	plot42 <- nmScatterPlot(testDataList[[1]], xVar = "X", yVar = "Y,W", bVar = "B", overlaid = TRUE, yAxisScaleRelations = "free")
	plot43 <- nmScatterPlot(testDataList[[1]], xVar = "X", yVar = "Y,W", bVar = "B", overlaid = TRUE, xAxisScaleRelations = "free",
			layout = c(1,2))
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot42))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot42.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot43))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot43.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3759 : add reference lines
test21 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	# check addition of reference lines
	testReflines <- list( c(0, 5), c(2, 0), 1, "a" )
	# non-overlaid plot
	plot44 <- nmScatterPlot(testDataList[[1]], "X", "Y", addLegend = TRUE,
			title = "Overlaid lines work for non-overlaid scatterplot", abLines = testReflines)
	# overlaid plot
	plot45 <- nmScatterPlot(testDataList[[1]], "X", "Y, Z", addLegend = TRUE, 
			title = "Overlaid lines work for overlaid scatterplot", overlaid = TRUE,
			abLines = testReflines, bVar = "G")
	# ignore bad abLines parameter (overlaid plot)
	plot46 <- nmScatterPlot(testDataList[[1]], "X", "Y, Z", addLegend = TRUE, 
			title = "Bad overlaid lines ignored for overlaid scatterplot", overlaid = TRUE,
			abLines = c(1,0), bVar = "G")
	# ignore bad abLines parameter (non-overlaid plot)
	plot47 <- nmScatterPlot(testDataList[[1]], "X", "Y, Z", addLegend = TRUE, 
			title = "Bad overlaid lines ignored for non-overlaid scatterplot", overlaid = FALSE,
			abLines = c(1,0), bVar = "G")
	# test loess line	
	oldLtySet <- getAllGraphParams()$superpose.line$lty
	setGraphParams("superpose.line", list(lty = rep(1:4, 2)))
	plot48 <- nmScatterPlot(subset(nmData(testDataList[[4]]), ID %in% 1:10), "TIME", "PRED, IPRED", 
			overlaid = TRUE, bVars = "ID", addLoess = TRUE, addLegend = TRUE, type = "p") 
	setGraphParams("superpose.line", list(lty = oldLtySet))
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot44))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot44.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot45))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot45.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot46))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot46.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot47))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot47.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot48))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterPlot48.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}





