#
genExpectedGraphs <- function(outputDir = "../expected/", writeImages = TRUE, 
		writeRData = FALSE, writeComparisonTable = FALSE, loadPack = TRUE)
{
	
	.dataList <- list()
	
	.dataList[[1]] <- data.frame(X = 1:10, Y = 1:10, Z = c(1:5, seq(from=6,to=7,length.out = 5)), 
			W = 10:1, G = rep(LETTERS[1:2], 5), B = rep(letters[1:2], each = 5))
	.dataList[[2]] <- data.frame(X = 1:20, Y = 1:20, Z = c(1:10, seq(from=11,to=12,length.out = 10)), 
			W = 20:1, G = rep(LETTERS[1:2], 10), B = rep(letters[1:2], each = 10))
	.dataList[[3]] <-  data.frame(X = 1:50, TIME = rep(1:25, 2), ID = rep(1:2, each = 25))
	
	.dataList[[4]] <- importNm(system.file(package = "RNMImport", "unittests/testdata/TestRun/TestData1.ctl"))
	
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
			"nmBarChart" = gen.nmBarChart(.dataList), "nmBoxPlot" = gen.nmDotPlot(.dataList),
			"nmDotPlot" = gen.nmDotPlot(.dataList))
	
	
	setGraphParams("plot.symbol", list(alpha = 1, cex = 1.5, col = "green", pch = 2))
	setGraphParams("plot.line", list(alpha = 1, col = "red", lwd = 2, "lty" = 2))
	setGraphParams("superpose.symbol", list(cex = c(0.8, 1.3), col = c("black", "red"), pch = c(19,2)))
	
	plots2 <- list("nmScatterPlot" = gen.nmScatterPlot(.dataList), "nmQQNorm" = gen.nmQQNorm(.dataList), 
			"nmBoxPlot" = gen.nmBoxPlot(.dataList), "nmScatterMatrix" = gen.nmScatterMatrix(.dataList), 
			"nmHistogram" = gen.nmHistogram(.dataList), "nmACPlot" = gen.nmACPlot(.dataList),
			"nmBarChart" = gen.nmBarChart(.dataList), "nmBoxPlot" = gen.nmDotPlot(.dataList),
			"nmDotPlot" = gen.nmDotPlot(.dataList))
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
		outtable <- data.frame(filePath1 = paste(getwd(), "/", sep = ""), filePath2 = outputDir,
				file1 = outfileNames, file2 = outfileNames, outFile = paste("_", outfileNames, sep = ""))
		write.csv(outtable, "testcases.csv", row.names = FALSE)
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
	plots[[1]] <- nmScatterPlot(testData, "X, W", c("Y", "Z"), bVar = "B", gVar = "G", addLegend = c(TRUE,FALSE,TRUE),
			idLines = c(TRUE, TRUE, FALSE, FALSE), titles = LETTERS[1:4], xLab = "X label", yLab = "yLabel", 
			pch = 1, equalYScales = FALSE)
	
	
	test2 <- nmScatterPlot(testData, "X", "Y, W", doPlot = FALSE, iVar = "G", types = c("p", "l"), addLoess = TRUE, 
			addGrid = c(FALSE, TRUE), titles = "BAR", pch = 19, equalYScales = FALSE)
	plots[[2]] <- test2
	
	testData2 <- testDataList[[2]]
	
	test3 <- nmScatterPlot(testData2, "X", "Y", doPlot = FALSE, bVar = "B")
	plots[[3]] <- test3
	# test very simple case
	test4 <- nmScatterPlot(testData2, "X", "Z", doPlot = FALSE)
	plots[[4]] <- test4
	
	test5 <- nmScatterPlot(testData2, "X", "Z", doPlot = FALSE, iVar = "G", type = "l")
	plots[[5]] <- test5
	
	test6 <- nmScatterPlot(testData2, "X", "Y", iVar = "B", type = "i")
	plots[[6]] <- test6
	plots[[7]] <- nmScatterPlot(testData2, "X", "Y", doPlot = FALSE, bVar = "B")
	# test equal axis scales
	plots[[8]] <-  nmScatterPlot(testData2, "X", "Z", doPlot = FALSE, equalAxisScales = TRUE)
	# test overlaid
	plots[[9]] <- nmScatterPlot(testData2, "X", "Y, Z", doPlot = FALSE,  overlaid = TRUE, idLine = TRUE, addLegend = TRUE )
	# test overlaid + bVar
	plots[[10]] <- nmScatterPlot(testDataList[[4]], "TIME", "PRED, IPRED", overlaid = TRUE, bVars = "SEX") 
	plots
}

gen.nmBoxPlot <- function(testDataList)
{
	testData <- testDataList[[1]]
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 3)
	plots[[1]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data")
	# check labels
	plots[[3]] <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", titles = "Sleep data",
			xLabs = "Group", yLabs = "Extra sleep")
	# different styles, "bVar"
	tmp <- getGraphParams("boxplot")
	setGraphParams("boxplot", list(fill = "darkgreen", col = "green", umb.col = "black", umb.lwd = 2 ) )
	CO2.df <- as.data.frame(CO2)
	plots[[2]] <- nmBoxPlot(CO2.df, contVar = "uptake", factVar = "Type", bVars = "Treatment")
	setGraphParams("boxplot", tmp)
	plots[[4]] <- nmBoxPlot(CO2.df, contVar = "uptake", factVar = "Type, Treatment")
	plots
}

gen.nmHistogram <- function(testDataList)
{
	testData <- testDataList[[1]]
	
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 4)
	test1 <- nmHistogram(testData, vars = "X, Z", bVar = "B", xLabs = c("HELLO", "WORLD"), titles = c("TITLE1", "TITLE2"))
	plots[[1]] <- test1
	
	test2 <- nmHistogram(testData, vars = "X, Z", bVar = "B", refLine = "median")
	plots[[2]] <- test2
	
	test3 <- nmHistogram(testData, vars = "Z", refLine = "mean")
	plots[[3]] <- test3
	
	test4 <- nmHistogram(testData, vars = "X, Z", bVar = "B", refLine = "median", addDensity = TRUE, type = "density")
	plots[[4]] <- test4
	
	plots
}

gen.nmScatterMatrix <- function(testDataList)
{
	testData2 <- testDataList[[2]]
	plots <- vector(mode = "list", length = 2)
	plots[[1]] <- nmScatterMatrix(testData2, "X,Y")
	# check the try/catch loess failure logic
	plots[[2]] <- nmScatterMatrix(testData2, "X,Y", addLoess = TRUE, title = "Foo")
	plots
}

gen.nmQQNorm <- function(testData)
{
	plots <- vector(mode = "list", length = 3)
	test1 <- nmQQNorm(sleep, "extra", bVar = "group", xLabs = "Normal distribution", titles = "Sleep data", yLabs = "Extra sleep")
	plots[[1]] <- test1
	# TODO: not working?
	oldSettings <- getGraphParams("refline")
	setGraphParams("refline", list(col = "black", lty = 2, lwd = 2))
	
	plots[[2]]<- nmQQNorm(sleep, "extra")
	plots[[3]] <- nmQQNorm(sleep, "extra", qqLine = FALSE)
	
	setGraphParams("refline", oldSettings)
	plots
}

gen.nmACPlot <- function(testDataList)
{
	testData3 <- testDataList[[3]]
	test1 <- nmACPlot(testData3, "X")
	list(test1)
}

gen.nmDotPlot <- function(testData)
{
	plots <- list()
	plots[[1]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W")
	plots[[2]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X,W", bVars = "G", title = "Test",
			 xLab = "xlabel", yLab = "ylabel")
	plots[[3]] <- nmDotPlot(testData[[1]], factVar = "B", contVar = "X", gVar = "G", title = "Test",
			 xLab = "xlabel", yLab = "ylabel")
	plots
	
}