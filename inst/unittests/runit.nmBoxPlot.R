
test.nmBoxPlot.df <- function()
{

	testPath <- .innerTestEnv$testScriptPaths[1]
	test1 <- nmBoxPlot(sleep, contVar = "extra" , factVar = "group", xLab = "Extra sleep", titles = "Sleep data")
	load(file.path(.innerTestEnv$expectedPath, "nmBoxPlot1.RData" ), envir = .innerTestEnv)
	checkEquals(test1, .innerTestEnv$x, "sleep data example, boxplot")
}