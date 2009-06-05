
runRNMGraphicsTests <- function(
		testDataPaths = c(system.file(package="RNMGraphics", "unittests"), 
				"\\\\Mango-data1\\mangowork\\MangoProjects\\RNONMEM2\\data"), 
		testScriptPaths = c(system.file(package="RNMGraphics", "unittests"), "ExternalTests/RNMGraphics"),
		runExtern = FALSE,
		printTestProtocol = TRUE,
		cleanup = TRUE
)
{
	stopifnot(require("RUnit", quietly = TRUE))
	results <- list()
	
	# allocated environment for use by tests
	.innerTestEnv <<- new.env()
	
	.innerTestEnv$testDataPaths <- testDataPaths
	.innerTestEnv$testScriptPaths <- testScriptPaths
	# linux has slighly different test case
	if(Sys.info()["sysname"] == "Linux" )
		.innerTestEnv$expectedPath <- file.path(testScriptPaths[1], "expected/linux")
	else
		.innerTestEnv$expectedPath <- file.path(testScriptPaths[1], "expected")
	# first, run the unit tests which are 
	testSuite <- defineTestSuite("Internal unit test suite", dirs = testScriptPaths[1],
			testFileRegexp = "^runit\\..+\\.[rR]$")
	res <- runTestSuite(testSuite)
	
	if(printTestProtocol)
		printHTMLProtocol(res, fileName = "RNMGraphics_internalunit.html" )
	
	results[["Internal unit"]] <- res
	
	# now run some test suites dependant on external data
	"%pst%" <- RNMImport:::"%pst%"
	
	if(runExtern)
	{
		.externTestEnv <<- new.env()
		# grab all of the test data at once
		assign("testRuns",externalTestRuns(testDataPaths[2]) , envir = .externTestEnv)
		
		if(is.na(testDataPaths[2]) || !file.exists(testDataPaths[2]))
		{
			RNMImportWarning("Unable to execute test suite since " %pst% testDataPaths[2] %pst% " does not exist\n",
					call = match.call())
			return(results)
		}
		setNmPath("testpath1", testDataPaths[2])
		testSuite <- defineTestSuite("External unit test suite", dirs = testScriptPaths[2], 
				testFileRegexp = "^runitextern1.+\\.[rR]$")
		res <- runTestSuite(testSuite)
		
		if(printTestProtocol)
			printHTMLProtocol(res, fileName = "external_unit1.html" )
		results[["External unit1"]] <- res
		if(cleanup)
		{
			rm(.externTestEnv, envir = .GlobalEnv)
			removeNmPath("testpath1")
		}
	}
	if(cleanup) rm(.innerTestEnv, envir = .GlobalEnv)
	results
	
}
