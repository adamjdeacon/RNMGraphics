# TODO: Add comment
# 
# Author: fgochez
###############################################################################


test.nmHistogram <- function()
{	
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.nmHistogram, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
					.RNMGraphicsTestEnv$imgExtension, "nmHistogram", 
					.RNMGraphicsTestEnv$testOutputPath, styles = .RNMGraphicsTestEnv$newStyles) 
			
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"nmHistogram",
			.RNMGraphicsTestEnv$imgExtension,.RNMGraphicsTestEnv$manifest["nmHistogram", "amount"]) 
			
	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")
}
