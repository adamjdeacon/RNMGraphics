# $LastChangedDate: 2010-08-25 10:53:09 +0100 (Wed, 25 Aug 2010) $
# $LastChangedBy: fgochez $
# $Rev: 20714 $
# 
# Author: fgochez
###############################################################################



test.nmACPlot <- function()
{
	
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.nmACPlot, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
			.RNMGraphicsTestEnv$imgExtension, "nmACPlot", 
			.RNMGraphicsTestEnv$testOutputPath, styles = .RNMGraphicsTestEnv$newStyles)
	
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"nmACPlot",	.RNMGraphicsTestEnv$imgExtension, 
			.RNMGraphicsTestEnv$manifest["nmACPlot", "amount"]) 
	
	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")
	
}
