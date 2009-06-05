# TODO: Add comment
# 
# Author: fgochez
###############################################################################

test.nmHistogram.df <- function()
{
	testPath <- .innerTestEnv$testScriptPaths[1]
	testData <- data.frame(X = 1:10, Y = 1:10, Z = c(1:5, seq(from=6,to=7,length.out = 5)), 
			W = 10:1, G = rep(LETTERS[1:2], 5), B = rep(letters[1:2], each = 5))
	test1 <- nmHistogram(testData, vars = "X, Z", bVar = "B", xLabs = c("HELLO", "WORLD"), titles = c("TITLE1", "TITLE2"))
	load(file.path(.innerTestEnv$expectedPath, "nmHistogram1.RData" ), envir = .innerTestEnv)
	checkEquals(test1, .innerTestEnv$x)
}

