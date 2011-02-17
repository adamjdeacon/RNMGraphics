# $LastChangedDate: 2010-08-25 10:53:09 +0100 (Wed, 25 Aug 2010) $
# $LastChangedBy: fgochez $
# $Rev: 20714 $
# 
# Author: fgochez
###############################################################################


test.timeEventPlots <- function()
{
	
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.timeEventPlots, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
			.RNMGraphicsTestEnv$imgExtension, "timeEventPlots", 
			.RNMGraphicsTestEnv$testOutputPath, styles = .RNMGraphicsTestEnv$newStyles)
	
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"timeEventPlots", .RNMGraphicsTestEnv$imgExtension, 
			.RNMGraphicsTestEnv$manifest["timeEventPlots", "amount"]) 
	
	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")
}


