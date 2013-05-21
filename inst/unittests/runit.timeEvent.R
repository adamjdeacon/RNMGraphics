# $LastChangedDate: 2010-08-25 10:53:09 +0100 (Wed, 25 Aug 2010) $
# $LastChangedBy: fgochez $
# $Rev: 20714 $
# 
# Author: fgochez
###############################################################################


test.timeEventPlots <- function()
{
	# check that graphs can be created (does NOT check that the graphs are actually correct)
	RNMGraphics:::createTestPlots(RNMGraphics:::gen.timeEventPlots, 
			.RNMGraphicsTestEnv$testDataList, 
			match.fun( .RNMGraphicsTestEnv$testDevice),
			.RNMGraphicsTestEnv$imgExtension, "timeEventPlots", 
			.RNMGraphicsTestEnv$testOutputPath, styles = .RNMGraphicsTestEnv$newStyles)
	
	x <- RNMGraphics:::getExpectedAndActual(.RNMGraphicsTestEnv$testOutputPath,"timeEventPlots", .RNMGraphicsTestEnv$imgExtension, 
			.RNMGraphicsTestEnv$manifest["timeEventPlots", "amount"]) 
	
	checkTrue(setequal( x$expected, x$actual), msg = " all expected graphs produced")

    # check error handling
    Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0)), Time2 = as.character(Time), conc2 = as.character(conc)))
    
    errors <- as.character(try(timeEventSPlot(Theoph2, tVar = "Time2", concVar = "conc", doseVar = "Dose", 
                    evtVar = "Evt", iVar = "Subject", expX = TRUE) ))
    errors[2] <- as.character(try(timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc2", doseVar = "Dose", 
                        evtVar = "Evt", iVar = "Subject", expY = TRUE) ))

    checkEquals(errors, c( "Error : Unable to exponentiate time variable\n",
                    "Error : Unable to exponentiate dependent variable\n"))
}


