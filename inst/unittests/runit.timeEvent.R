

test01 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	x <- nmData(testData[[4]])
	y <- subset(x, ID %in% 2:4)
	plot1 <- timeEventSPlot(x, title = "Time/event", xLab = "Time", yLab = "Concentration",
			subjectNum = 2:4)
	plot2 <- timeEventDPlot(y, title = "Time/event", xLab = "Time", yLab = "Subject")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot1))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots1.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot2))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots2.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}


# test styles
test02 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	x <- nmData(testData[[4]])
	y <- subset(x, ID %in% 2:4)
	oldSettings <- getAllGraphParams()
	setAllGraphParams(.RNMGraphicsTestEnv$newStyles)
	plot3 <- timeEventSPlot(y, title = "Time/event", xLab = "Time", yLab = "Concentration", layout = c(3,1))
	plot4 <- timeEventDPlot(y, title = "Time/event", xLab = "Time", yLab = "Subject")
	setAllGraphParams(oldSettings)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot3))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots3.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot4))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots4.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
}

test03 <- function() {
	testData <- .RNMGraphicsTestEnv$testDataList
	x <- nmData(testData[[4]])
	y <- subset(x, ID %in% 2:4)
	plot5 <- timeEventSPlot(y, title = "Time/event", xLab = "Time", yLab = "Concentration", layout = c(3,1),
			yAxisScaleRelations = "free")
	plot6 <- timeEventSPlot(y, title = "Time/event", xLab = "Time", yLab = "Concentration", layout = c(1,3),
			expX = TRUE, xRotAngle = 45)
	plot7 <- timeEventSPlot(y, expY = TRUE, xAxisScaleRelations = "free")
	Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0))))
	plot8 <- timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc", doseVar = "Dose", evtVar = "Evt", iVar = "Subject")
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot5))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots5.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot6))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots6.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot7))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots7.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
	
	MD5_plot <- RNMGraphics:::getMD5(show(plot8))
	MD5_expected <- .RNMGraphicsTestEnv$ExpectedMD5[["timeEventPlots8.jpg"]]
	checkEquals(MD5_plot, MD5_expected)
}


# check error handling
test04 <- function() {
	
	Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0)), Time2 = as.character(Time), conc2 = as.character(conc)))
	
	errors <- as.character(try(timeEventSPlot(Theoph2, tVar = "Time2", concVar = "conc", doseVar = "Dose", 
							evtVar = "Evt", iVar = "Subject", expX = TRUE) ))
	errors[2] <- as.character(try(timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc2", doseVar = "Dose", 
							evtVar = "Evt", iVar = "Subject", expY = TRUE) ))
	
	checkEquals(errors, c( "Error : Unable to exponentiate time variable\n",
					"Error : Unable to exponentiate dependent variable\n"))
	
}


# check error handling
test05 <- function() {
	
	Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0)), Time2 = as.character(Time), conc2 = as.character(conc)))
	
	errors <- as.character(try(timeEventSPlot(Theoph2, tVar = "Time2", concVar = "conc", doseVar = "Dose", 
							evtVar = "Evt", iVar = "Subject", expX = TRUE) ))
	errors[2] <- as.character(try(timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc2", doseVar = "Dose", 
							evtVar = "Evt", iVar = "Subject", expY = TRUE) ))
	
	checkEquals(errors, c( "Error : Unable to exponentiate time variable\n",
					"Error : Unable to exponentiate dependent variable\n"))
	
}





