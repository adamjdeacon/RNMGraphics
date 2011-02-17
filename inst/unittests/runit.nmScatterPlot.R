
test.nmScatterPlot <- function()
{
	
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.nmScatterPlot, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
			.RNMGraphicsTestEnv$imgExtension, "nmScatterPlot", 
			.RNMGraphicsTestEnv$testOutputPath, .RNMGraphicsTestEnv$newStyles )
	
	# getExpectedAndActual <- function(outPath, plotName, imgExtension, expectedNum)
	
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"nmScatterPlot",	.RNMGraphicsTestEnv$imgExtension, 
			.RNMGraphicsTestEnv$manifest["nmScatterPlot", "amount"]) 
	
	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")
}
