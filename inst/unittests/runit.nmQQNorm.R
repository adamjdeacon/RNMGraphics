
test01 <- function() {
	
	plot1 <- nmQQNorm(sleep, "extra", bVar = "group", xLabs = "Normal distribution", titles = "Sleep data", yLabs = "Extra sleep")
	plot2 <- nmQQNorm(sleep, "extra")
	plot3 <- nmQQNorm(sleep, "extra", qqLine = FALSE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	
}


# okimberlin: tests the scale argument
test02 <- function() {
	
	plot4 <- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="free")
	plot5 <- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="sliced")
	plot6 <- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="same")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test generic, etc
test03 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testData[[4]]@problems[[1]]) <- "ID < 10"
	plot7 <- nmQQNorm(testData[[4]], "IWRES", bVars = "ID,TIME",
			maxTLevels = 2,maxPanels = 9 )
	plot8 <- nmQQNorm(getProblem(testData[[4]]), "IWRES", bVars = "ID,TIME",
			maxTLevels = 2,maxPanels = 9 )
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test styling
test04 <- function() {
	
	oldSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot9 <- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScaleRelations="free", title = "Swiss", yLab = "Fert and Mort", xLab = "Norm")
	setAllGraphParams(oldSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot9))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm9.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# (issue 3741) test xAxisScaleRelations
test05 <- function() {
	
	ChickWeight.df <- as.data.frame(ChickWeight)
	plot10 <- nmQQNorm(ChickWeight.df,"weight", bVar =  "Diet", xAxisScaleRelations="free")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot10))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmQQNorm10.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}



	
	
	
	


