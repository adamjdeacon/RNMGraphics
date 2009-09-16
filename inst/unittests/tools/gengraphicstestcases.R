
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

gen.nmBarChart <- function(testData)
{
	plots <- vector(mode = "list", length = 2)
	CO2.df <- as.data.frame(CO2)
	plots[[1]] <- nmBarChart(CO2.df, "Plant", "Treatment", title = "CO2", xLab = "The Treatment", 
			bVar ="Type" , yLab = "The type")
	
	plots[[2]] <- nmBarChart(CO2.df, "Treatment", "Type, Plant" )
	
	plots
}

gen.nmScatterPlot <- function(testDataList)
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
	plots[[6]] <-  nmScatterPlot(testDataList[[1]], "X", "Y, W", bVar = "B", gVar = "G", type = "p", addLegend = TRUE, yAxisRelations = "free")
	
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
	plots[[15]] <- nmScatterPlot(testDataList[[7]], "Y", "X", titles = "X axis should be integer only")
	plots[[16]] <- nmScatterPlot(testDataList[[7]], "X", "Y,Z", titles = "Two y variables, behavior is ignored", yAxisRelations = "free")
	
	nmPlotData <- nmData(testDataList[[4]])
	
	nmPlotData <- addDerivedCategorical(nmPlotData, "TIME", "TIME.CUT")
	graphSubset(nmPlotData) <- "ID < 10"
	plots[[17]] <- nmScatterPlot(nmPlotData, "TIME.CUT", "PRED, IPRED", xRotAngle=90, bVar = "SEX", logY = TRUE)
	plots[[18]] <- nmScatterPlot(nmPlotData, "TIME.CUT", "PRED, IPRED", xRotAngle=90, bVar = "SEX", logY = TRUE, overlaid = TRUE)
	
	# logX test needed
	plots[[19]] <- nmScatterPlot(nmPlotData, yVars = "DV", xVars =  "TIME", logX = TRUE, type = "l")
	# logX and logY
	plots[[20]] <- nmScatterPlot(nmPlotData, yVars = "DV", xVars = "TIME", logX = TRUE, logY = TRUE, type = "l")
	plots
	
}

gen.nmBoxPlot <- function(testDataList)
{
	testData <- testDataList[[1]]
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 3)
	
	# check labels / simple plot
	plots[[1]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep")
	
	CO2.df <- as.data.frame(CO2)
	
	# free y axis relations, bVar
	
	plots[[2]] <- nmBoxPlot(CO2.df, contVar = "uptake, conc", factVar = "Type", bVars = "Treatment", 
			yAxisRelations = "free")
	# continuous variable on x axis
	plots[[3]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep", contVarOnX = TRUE)
		
	
	graphSubset(testDataList[[4]]@problems[[1]]) <- "ID < 10"
	
	# finally, check maxTLevels and maxPanels, along with generics and subset and factBin, xRotAngle
	plots[[4]] <- nmBoxPlot(testDataList[[4]], contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "ID,TIME", maxTLevels = 2, maxPanels = 9, xRotAngle = 90)
	plots[[5]] <- nmBoxPlot(getProblem(testDataList[[4]],1), contVar = "WRES", factVar = "PRED", factBin = 3,
			bVar = "ID,TIME", maxTLevels = 2, maxPanels = 9, xRotAngle = 90)
	plots
}

gen.nmHistogram <- function(testDataList)
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
	
	# finally, check maxTLevels and maxPanels, along with generics and subset and factBin, xRotAngle
	
	
	plots
}


gen.nmScatterMatrix <- function(testDataList)
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
	plots
}

gen.nmQQNorm <- function(testData)
{
	plots <- vector(mode = "list", length = 3)
	test1 <- nmQQNorm(sleep, "extra", bVar = "group", xLabs = "Normal distribution", titles = "Sleep data", yLabs = "Extra sleep")
	plots[[1]] <- test1
	
	
	plots[[2]]<- nmQQNorm(sleep, "extra")
	plots[[3]] <- nmQQNorm(sleep, "extra", qqLine = FALSE)
	
	
	#####okimberlin: tests the scale argument
	
	plots[[4]]<- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScales="free")
	plots[[5]]<- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScales="sliced")
	plots[[6]]<- nmQQNorm(swiss,c("Fertility","Infant.Mortality"),yAxisScales="same")
	
	# test generic, etc 
	graphSubset(testData[[4]]@problems[[1]]) <- "ID < 10"
	plots[[7]] <- nmQQNorm(testData[[4]], "IWRES", bVars = "ID,TIME",
			maxTLevels = 2,maxPanels = 9 )
	plots[[8]] <- nmQQNorm(getProblem(testData[[4]]), "IWRES", bVars = "ID,TIME",
			maxTLevels = 2,maxPanels = 9 )
	plots
}

# minimal test, other features should be tested by nmScatterPlot tests

gen.nmACPlot <- function(testDataList)
{
	testData3 <- testDataList[[3]]
	plots <- list()
	plots[[1]]<- nmACPlot(testData3, "X")
	
	plots
}

gen.nmDotPlot <- function(testData)
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

	plots
	
}

gen.timeEventPlots <- function(testData)
{
	x <- nmData(testData[[4]])
	x <- subset(x, ID %in% 2:4)
	plots <- list()
	plots[[1]] <- timeEventSPlot(x, title = "Time/event", xLab = "Time", yLab = "Concentration")
	plots[[2]] <- timeEventDPlot(x, title = "Time/event", xLab = "Time", yLab = "Subject")
	
	plots
}