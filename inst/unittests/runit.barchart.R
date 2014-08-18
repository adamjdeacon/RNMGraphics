

test01 <- function() {
	CO2.df <- as.data.frame(CO2)
	plot1 <- nmBarChart(CO2.df, "Plant", "Treatment", title = "CO2", xLab = "The Treatment", yLab = "The type")
	plot2 <- nmBarChart(CO2.df, "Treatment", "Plant", bVar = "Type" )
	
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}



# tests xRotAngle, binning of x-axis (explicit cuts), by variable (issue 1156)
test02 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	plot3 <- nmBarChart(testData[[1]], xBin = "0,3,7,10", xVar = "X", yVar = "G", bVar = "B", xRotAngle = 90  )	
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# non-explicit cut, multiple by-vars (issue 1156)
test03 <- function() {

	plot4 <- nmBarChart(mtcars, xVar = "mpg", yVar = "carb", bVar = "vs,am", xBin = 2)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# 3022 - commas should be allowed in axis labels
test04 <- function() {
	CO2.df <- as.data.frame(CO2)
	plot5 <- nmBarChart( CO2.df, "Treatment", "Plant", xLab = "a,b", yLab = "a,b" )
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# test styles
test05 <- function() {
	CO2.df <- as.data.frame(CO2)
	oldGraphSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot6 <- nmBarChart(CO2.df, "Treatment", "Plant", bVars = "Treatment", xLab = "xlab", yLab = "ylab", title = "style test")
	setAllGraphParams(oldGraphSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# issue 3741
test06 <- function() {
	CO2.df <- as.data.frame(CO2)
	plot7 <- nmBarChart(CO2.df, "Treatment", "Plant", bVar = "Type", yAxisScaleRelations = "free" )
	plot8 <- nmBarChart(CO2.df, "Treatment", "Plant", bVar = "Type", xAxisScaleRelations = "free", layout = c(1,2) )    
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["nmBarChart8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}




