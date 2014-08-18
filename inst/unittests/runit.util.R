

# tests the "processTrellis" function
test.processTrellis <- function()
{
	testDf <- data.frame( ID = rep(1:5, times = 2), X = seq(from = 1.1, to = 10.1), Y = letters[1:10], Z = seq(from = 1.1, to = 10.1), W = 1:10)
	
	# test "standard" call (ignores non-numeric as well)
	
	test1 <- RNMGraphics:::processTrellis( testDf, columns = c("X","ID","Y"), postFix = "cut", maxLevels = 5 )
	expected1 <- addDerivedCategorical(testDf, "X", newVar = "X.cut", breaks = 5, binType = "counts")
	
	checkEquals(test1$data, expected1, msg = " basic call executed correctly")
	
	# maxLevels will be comma seperated characters for explicit breaks
	
	test2 <- RNMGraphics:::processTrellis( testDf, columns = c("X","ID"), maxLevels = "1,2,3,11" )
	expected2 <- addDerivedCategorical( testDf, "X", newVar = "X", breaks = c(1,2,3,11), binType = "explicitcuts" )
	checkEquals(test2$data, expected2, msg = " explicit cuts correct")
	
	# check non-default exempt columns
	
	test3 <- RNMGraphics:::processTrellis( testDf, columns = c("X","ID"), maxLevels = 4, exemptColumns = "X" )
	expected3 <- addDerivedCategorical(testDf, "ID", "ID", breaks = 4, binType = "counts") 
	checkEquals(test3$data, expected3, msg = " exempt columns correct" )
	
	# regression test for issue 3103
	# integer columns should be checked
	
	test4 <- RNMGraphics:::processTrellis( testDf, columns = "W", postFix = "cut", maxLevels = 2)
	expected4 <- addDerivedCategorical(testDf, "W", "W.cut", breaks = 2, binType = "counts") 
	checkEquals(test4$data, expected4)
	
	
}