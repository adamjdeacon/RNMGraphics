

test01 <- function() {
	plot1 <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", medianLines = FALSE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check labels / simple plot
test02 <- function() {
	CO2.df <- as.data.frame(CO2)
	plot2 <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", bVars = "Treatment", 
			yAxisScaleRelations = "free", medianLines = FALSE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# continuous variable on x axis
test03 <- function() {
	
	plot3 <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", contVarOnX = TRUE, medianLines = FALSE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check maxTLevels and maxPanels, along with generics and subset and factBin, xRotAngle
test04 <- function() {
	
	testDataList <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	
	plot4 <- nmBoxPlot(testDataList[[4]], contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "ID,TIME", maxTLevels = 2, maxPanels = 9, xRotAngle = 90, medianLines = FALSE, modifyOnResidual = FALSE)
	plot5 <- nmBoxPlot(getProblem(testDataList[[4]],1), contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "ID,TIME", maxTLevels = 2, maxPanels = 9, xRotAngle = 90, medianLines = FALSE, modifyOnResidual = FALSE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test balanced axes, horizontal lines, median lines
# issue 2310
test05 <- function() {
	
	plot6 <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", 
			medianLines = TRUE, balancedContAxis = TRUE, hLines = c(-1,0,2))
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# same as before, but with continuous variable on x-axis
test06 <- function() {
	
	plot7 <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", contVarOnX = TRUE,  
			medianLines = TRUE, balancedContAxis = TRUE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# customized residual plot, should force balanced continuous axes and reference line at x = 0
# issue 2310
test07 <- function() {
	
	testDataList <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"

	plot8 <- nmBoxPlot(testDataList[[4]], contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "TIME", maxTLevels = 2, xRotAngle = 90, modifyOnResidual = TRUE, layout = c(3, 1))
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# same as above, different residual variables allowed
# issue 2310
test08 <- function() {
	
	testDataList <- .RNMGraphicsTestEnv$testDataList
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"

	plot9 <- nmBoxPlot(testDataList[[4]], contVar = "RES", factVar = "SEX",modifyOnResidual = TRUE, 
			residVars = "RES")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot9))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot9.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3032 - commas should be allowed in axis labels
test09 <- function() {
	
	plot10 <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "a,b", yLabs = "a,b", medianLines = FALSE)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot10))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot10.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test styles
test10 <- function() { 
	CO2.df <- as.data.frame(CO2)
	oldGraphSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot11 <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", bVars = "Treatment", yAxisScaleRelations = "free", medianLines = FALSE, hLines = 400)
	setAllGraphParams(oldGraphSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot11))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot11.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3741
test11 <- function() {
	
	CO2.df <- as.data.frame(CO2)
	plot12 <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", 
			bVars = "Treatment", contVarOnX = TRUE,  xAxisScaleRelations = "free", medianLines = FALSE, hLines = 400)    
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot12))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBoxPlot12.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}





