
test01 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData2 <- testDataList[[2]]
	
	plot1 <- nmScatterMatrix(testData2, "X,Y", bVars = "B")

	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterMatrix1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check the try/catch loess failure logic
test02 <- function() {
	testDataList <- .RNMGraphicsTestEnv$testDataList
	testData2 <- testDataList[[2]]
	plot2 <- nmScatterMatrix(testData2, "X,Y", addLoess = TRUE, title = "Foo")
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	plot3 <- nmScatterMatrix(testDataList[[4]], 
			"DV,PRED,IPRED","TIME,ID", iVar = "SUBJ", maxTLevels = 4, maxPanel = 9)
	plot4 <- nmScatterMatrix(getProblem(testDataList[[4]]), 
			"DV,PRED,IPRED","TIME,ID", iVar = "SUBJ", maxTLevels = 4, maxPanel = 9)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterMatrix2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterMatrix3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterMatrix4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test styles
test03 <- function() {
	
	testDataList <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	oldGraphSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot5 <- nmScatterMatrix(testDataList[[4]], "DV,PRED", addLoess = TRUE, title = "Foo", bVar = "SEX")
	setAllGraphParams(oldGraphSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmScatterMatrix5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}





