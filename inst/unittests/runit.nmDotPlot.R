
test01 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	plot1 <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W")
	plot2 <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", bVars = "G", title = "Test 2",
			xLab = "xlabel", yLab = "ylabel")
	plot3 <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X", gVar = "G", title = "Test 3",
			xLab = "xlabel", yLab = "ylabel")
	plot4 <- nmDotPlot(testData[[1]], factVar = "Y", contVar = "X", title = "Test 4",
			xLab = "xlabel", yLab = "ylabel", maxFactPerPanel = 5)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# check maxFactPerPanel, generics, etc
test02 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testData[[4]]@problems[[1]]) <- "ID < 10"
	
	plot5 <- nmDotPlot(testData[[4]], factVar = "ID", contVar = "WT", bVars = "TIME", 
			maxTLevels = 4, title = "Test 5", gVar = "SEX", addLegend = TRUE )
	plot6 <- nmDotPlot(getProblem(testData[[4]]), factVar = "ID", contVar = "WT", bVars = "TIME", 
			maxTLevels = 4, title = "Test 5", gVar = "SEX", addLegend = TRUE )
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# Tests that the maxFactPerPanel algorithm has now been corrected.
test03 <- function() {

	testDf <- data.frame(X = rep(1:2, each = 10), ID = rep(1:10, length.out = 20))
	
	plot7 <- nmDotPlot(testDf, factVar = "ID", contVar = "X", maxFactPerPanel = 4)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# tests fix for issue 3032 - commas should be allowed on axes
test04 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	
	plot8 <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W",
			yLab = "a,b", xLab = "a,b")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# tests fix for issue 3098 : auxilliary trellis variable created for "maxFactPerPanel" should never be binned 
test05 <- function() {

	testDf2 <- data.frame(X = rep(1, 100), ID = 1:100)
	
	plot9 <- nmDotPlot(testDf2, contVar = "X", factVar = "ID", title = "ID.GRP is not binned", maxFactPerPanel = 5,
			maxTLevels = 8)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot9))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot9.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test styles
test06 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	oldSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot10 <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", bVars = "G", title = "Test 2",
			xLab = "xlabel", yLab = "ylabel")
	setAllGraphParams(oldSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot10))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot10.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3741 : adding scale setting option
test07 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList

	plot11  <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", yAxisScaleRelations = "free") 
	plot12  <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", xAxisScaleRelations = "free")
	plot13  <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", 
			xAxisScaleRelations = "free", yAxisScaleRelations = "free")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot11))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot11.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot12))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot12.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot13))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmDotPlot13.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}




