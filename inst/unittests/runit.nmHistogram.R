


test01 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData <- testDataList[[1]]
	testData2 <- testDataList[[2]]
	
	plot1 <- nmHistogram(testData, vars = "X, Z", bVar = "B", xLabs = "HELLO", titles = "TITLE")
	plot2 <- nmHistogram(testData, vars = "X, Z", bVar = "B", refLine = "median")
	plot3 <- nmHistogram(testData, vars = "Z", refLine = "mean")
	plot4 <- nmHistogram(testData, vars = "X, Z", bVar = "B", refLine = "median", addDensity = TRUE, type = "density")

	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# test styles
test02 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData <- testDataList[[1]]
	
	oldGraphSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot5 <- nmHistogram(testData, vars = "X, Z", bVar = "B", xLabs = "HELLO", titles = "TITLE", addDensity = TRUE, type = "density")
	setAllGraphParams(oldGraphSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check maxTLevels and maxPanels, along with generics and subset
test03 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData3 <- testDataList[[4]]
	graphSubset(testData3) <- "ID < 10"
	plot6 <- nmHistogram(testData3, "WRES, IWRES", bVar = "AGE", maxTLevels = 3, maxPanels = 4)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check breaks 
test04 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData <- testDataList[[1]]
	plot7 <- nmHistogram(testData, "Z", breaks = c(1, 5, 7))
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test x and y axis scale adjustment (issue 3741)
test05 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData <- testDataList[[1]]
	
	plot8 <- nmHistogram(testData, vars = "X, Z", 
			bVar = "B", xLabs = "HELLO", titles = "TITLE" ,
			xAxisScaleRelations = "free")
	plot9 <- nmHistogram(testData, vars = "X, Z", 
			bVar = "B", xLabs = "HELLO", titles = "TITLE", 
			yAxisScaleRelations = "free")
	plot10 <- nmHistogram(testData, vars = "X, Z", 
			bVar = "B", xLabs = "HELLO", titles = "TITLE", 
			yAxisScaleRelations = "free", xAxisScaleRelations = "free")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot9))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram9.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot10))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmHistogram10.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}





