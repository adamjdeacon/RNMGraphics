# $LastChangedDate: 2010-08-25 10:53:09 +0100 (Wed, 25 Aug 2010) $
# $LastChangedBy: fgochez $
# $Rev: 20714 $
# 
# Author: fgochez
###############################################################################

test.nmBoxPlot <- function()
{	
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.nmBoxPlot, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
					.RNMGraphicsTestEnv$imgExtension, "nmBoxPlot", 
					.RNMGraphicsTestEnv$testOutputPath, styles = .RNMGraphicsTestEnv$newStyles) 
			
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"nmBoxPlot",	.RNMGraphicsTestEnv$imgExtension, 
					.RNMGraphicsTestEnv$manifest["nmBoxPlot", "amount"]) 

	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")
}