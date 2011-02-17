
test.nmQQNorm <- function()
{	
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.nmQQNorm, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
			.RNMGraphicsTestEnv$imgExtension, "nmQQNorm", 
			.RNMGraphicsTestEnv$testOutputPath, styles = .RNMGraphicsTestEnv$newStyles) 
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"nmQQNorm",	.RNMGraphicsTestEnv$imgExtension, 
			.RNMGraphicsTestEnv$manifest["nmQQNorm", "amount"]) 
	
	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")
}
