
test01 <- function() {
	testData3 <- .RNMGraphicsTestEnv$testDataList[[3]]
	plot1 <- nmACPlot(testData3, "X")

	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmACPlot1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test styling
test02 <- function() {
	
	testData3 <- .RNMGraphicsTestEnv$testDataList[[3]]
	oldSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	testData <- cbind(testData3, G = rep(c("A","B"), times = 25))
	plot2 <- nmACPlot(testData, "X", timeVar = "TIME", iVar = "ID", gVar = "G", xLab = "-X-", yLab = "X lagged",
			title = "Style test", addLegend = TRUE)
	plot3 <- nmACPlot(testData, "X", timeVar = "TIME", iVar = "ID", bVar = "G", xLab = "-X-", yLab = "X lagged",
			title = "Style test 2")
	setAllGraphParams(oldSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmACPlot2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmACPlot3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}




