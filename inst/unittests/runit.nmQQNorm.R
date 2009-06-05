
test.nmQQNorm.df <- function()
{
	testPath <- .innerTestEnv$testScriptPaths[1]
	test1 <- nmQQNorm(sleep, "extra", bVar = "group", xLabs = "Normal distribution", titles = "Sleep data", yLabs = "Extra sleep")
	load(file.path(.innerTestEnv$expectedPath, "nmQQNorm1.RData" ), envir = .innerTestEnv)
	checkEquals(test1, .innerTestEnv$x, "sleep data example")
}
