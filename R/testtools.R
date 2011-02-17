# SVN revision: $Rev: 24110 $
# Date of last change: $LastChangedDate: 2011-01-10 16:45:38 +0000 (Mon, 10 Jan 2011) $
# Last changed by: $LastChangedBy: fgochez $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' 
#' @param generatorFun 
#' @param testData 
#' @param devFun 
#' @param imgExtension 
#' @param plotPrefix 
#' @param outPath 
#' @title
#' @return 
#' @author fgochez
#' @export


createTestPlots <- function( generatorFun, testData, devFun, imgExtension, plotPrefix, 
							outPath, styles = getAllGraphParams())
{
	plots <- generatorFun( testData, styles )
	
	for(i in seq_along(plots))
	{
		# output to files named vpc1.png, vpc2.png, etc. (replace png with relevant device extensionm)
		
#		devFun(filename = file.path(outPath, 
#						paste( paste(plotPrefix, i,sep = ""), 							
#								imgExtension, sep = "." )) )
		devCall <- c( list( filename = file.path(outPath, paste( paste(plotPrefix, i,sep = ""), 							
										imgExtension, sep = "." )) ), 
				.RNMGraphicsTestEnv$deviceSettings )
		do.call(devFun, devCall) 
		show(plots[[i]])
		dev.off()
	}
}

#' 
#' @param outPath 
#' @param plotName 
#' @param imgExtension 
#' @param expectedNum 
#' @title
#' @return 
#' @author fgochez
#' @export

getExpectedAndActual <- function(outPath, plotName, imgExtension, expectedNum)
{
	actualFiles <- dir( path = outPath, pattern = paste(paste("^", plotName, "[0-9]+", sep = ""), 
					sep = "\\.", imgExtension ), full = FALSE )
	expectedFiles <- paste( plotName, 1:expectedNum, ".", imgExtension, sep = "")
	list(actual = actualFiles, expected = expectedFiles )
}
