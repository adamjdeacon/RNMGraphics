
test.nmScatterPlot.df <- function()
{
	testPath <- .innerTestEnv$testScriptPaths[1]
	testData <- data.frame(X = 1:10, Y = 1:10, Z = c(1:5, seq(from=6,to=7,length.out = 5)), 
	W = 10:1, G = rep(LETTERS[1:2], 5), B = rep(letters[1:2], each = 5))
	test1 <- nmScatterPlot(testData, "X, W", c("Y", "Z"), bVar = "B", gVar = "G", addLegend = c(TRUE,FALSE,TRUE),
					idLines = c(TRUE, TRUE, FALSE, FALSE), titles = LETTERS[1:4], xLab = "X label", yLab = "yLabel", 
						pch = 1)
	load(file.path(.innerTestEnv$expectedPath, "nmScatterPlot1.RData" ), envir = .innerTestEnv)
				checkEquals(test1, .innerTestEnv$x)
	test2 <- nmScatterPlot(testData, "X", "Y, W", doPlot = FALSE, iVar = "G", types = c("p", "l"), addLoess = TRUE, 
				addGrid = c(FALSE, TRUE), titles = c("BAR"), pch = 19)
	
	load(file.path(.innerTestEnv$expectedPath, "nmScatterPlot2.RData" ), envir = .innerTestEnv)
	checkEquals(test2, .innerTestEnv$x)
	
	testData2 <- data.frame(X = 1:20, Y = 1:20, Z = c(1:10, seq(from=11,to=12,length.out = 10)), 
			W = 20:1, G = rep(LETTERS[1:2], 10), B = rep(letters[1:2], each = 10))
	# check plotting in which one loess fails, but is ignored
	test3 <- nmScatterPlot(testData2, "X", "Y", doPlot = FALSE, bVar = "B")
	load(file.path(.innerTestEnv$expectedPath, "nmScatterPlot3.RData" ), envir = .innerTestEnv)
	
	checkEquals(test3, .innerTestEnv$x)
	
}
