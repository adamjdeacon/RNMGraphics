
NUMSUPERPOSELEVELS <- 7

setupTestEnv <- function(testDataPath = "testdata", graphOutputPath = "graphtests")
{
	.RNMGraphicsTestEnv <<- new.env()
	# path where the test data should be found
	.RNMGraphicsTestEnv$testDataPath <- testDataPath
	.RNMGraphicsTestEnv$testOutputPath <- graphOutputPath
	.RNMGraphicsTestEnv$testDevice <- "jpeg"
	.RNMGraphicsTestEnv$imgExtension <- "jpg"
	.RNMGraphicsTestEnv$deviceSettings <- list(width = 400, height = 400, quality = 75)
	
	# load a manifest of expected plots
	
	.RNMGraphicsTestEnv$manifest <- read.csv( system.file( package = "RNMGraphics", "unittests/tools/manifest.csv"  ), 
			stringsAsFactors = FALSE, row.names = 1 )
	
	# load test data list
	
	.dataList <- list()
	
	.dataList[[1]] <- data.frame(X = 1:10, Y = 1:10, Z = c(1:5, seq(from=6,to=7,length.out = 5)), 
			W = 10:1, G = rep(LETTERS[1:2], 5), B = rep(letters[1:2], each = 5))
	.dataList[[2]] <- data.frame(X = 1:20, Y = 1:20, Z = c(1:10, seq(from=11,to=12,length.out = 10)), 
			W = 20:1, G = rep(LETTERS[1:2], 10), B = rep(letters[1:2], each = 10))
	.dataList[[3]] <-  data.frame(X = 1:50, TIME = rep(1:25, 2), ID = rep(1:2, each = 25))
	
	.dataList[[4]] <- importNm("TestData1.ctl", path = file.path( testDataPath, "TestRun"))
	.dataList[[5]] <- data.frame(X = rnorm(100), Y = 1)
	.dataList[[6]] <- data.frame(X = rep(seq(1, 2, length.out = 4), times = 4) + rep(c(0, 5, 10, 15), each = 4 ), Y = rep(1:4, times = 4),
			G = rep(letters[1:4], each = 4), H = rep(1:2, each = 8))
	
	# final data set for checking algorithm to force integer axes to only have integer tick marks
	.dataList[[7]] <- data.frame(Y = c(2, 3, 1, 3, 3, 1, 3, 2, 1, 3), 
			X = c(1.3520803, 0.6580209, -1.3252070, 0.6388983, 0.4507879,  
					0.5380433, 1.1361281, 0.3263466, 1.2457650, 0.9912256), 
			Z = c(1, 3, 1, 2, 4, 2, 4, 1, 2, 1))	
	
	.dataList[[8]] <- data.frame( X1 = seq(from = 1, to = 10, by = 0.5), X2 = seq(from = -4, to = 5, by = 0.5), 
			Y1 = seq(from = 10, to = 1, by = -0.5), Y2 = seq(from = 5, to = -4, by = -0.5) )
	
	.RNMGraphicsTestEnv$testDataList <- .dataList
	
	
	# create an alternative set of stylings to test styling
	
	newStyles <- getAllGraphParams()
	
	newStyles$superpose.symbol <- list( cex = rep(1.5, NUMSUPERPOSELEVELS), col = terrain.colors( NUMSUPERPOSELEVELS ), 
										pch = 1:NUMSUPERPOSELEVELS )
	newStyles$superpose.line <- list( alpha = 1, col = terrain.colors(NUMSUPERPOSELEVELS), 
			lty = rep(1:3, length.out = NUMSUPERPOSELEVELS), lwd = seq(from = 1, to = 3, length.out = NUMSUPERPOSELEVELS)  )

	newStyles$plot.symbol <- list( alpha = 1, cex = 1.5, col = "black", fill = "gray", 
			pch = 19)
	
	newStyles$plot.line <- list(lwd = 2, lty = 2, col = "red", alpha = 1)
	
	newStyles$plot.text <- list( alpha = 1, cex = 2, col = "purple" )
	newStyles$superpose.text <- list( alpha = rep(1, NUMSUPERPOSELEVELS), cex = rep(c(1, 1.5, 2), length.out = NUMSUPERPOSELEVELS),
			 col = terrain.colors(NUMSUPERPOSELEVELS))
	
	newStyles$loess.line <- list( lwd = 1, col = "lightblue" ) 
	
	newStyles$barchart <- list( alpha = rep(1, NUMSUPERPOSELEVELS), lty = rep(1:4, length.out = NUMSUPERPOSELEVELS), 
			lwd = seq(from = 1, to = 3, length.out = NUMSUPERPOSELEVELS), border = terrain.colors(NUMSUPERPOSELEVELS), 
			col = heat.colors(NUMSUPERPOSELEVELS))
	
	newStyles$histogram <- list( alpha = 1, col = "white", border = "black", lty = 2, lwd = 2,
			dens.col = "green", dens.lty = 2, dens.lwd = 2)
	
	newStyles$refline <- list(alpha = 1, col = "black", lty = 2, lwd = 2)
	
	newStyles$axis.text <- list(alpha = 1, cex = 1.5, col = "steelblue", font = 2)
	newStyles$boxplot <- list(alpha = 1, col = "cyan", fill = "blue", lty = 2, lwd = 2, 
			umb.col = "red", umb.lty = 2, umb.lwd = 2)
	
	newStyles$title.text <- list( alpha = 1, cex = 2, col = "green", font = 3, lineheight = 2 )
	
	newStyles$grid <- list(col = "blue", lty = 2, lwd = 2)
	
	newStyles$strip.bg <-  list(col = terrain.colors(NUMSUPERPOSELEVELS))
	
	#	$legend
#	$legend$position
#	[1] "right"
#	
#	$legend$cex
#	[1] 0.7
#	
#	$legend$maxTitleLength
#	[1] Inf

	newStyles$legend <- list(position = "top", cex = 1, maxTitleLength  = 7)
	
	.RNMGraphicsTestEnv$newStyles <- newStyles

#	$layout.widths
#	$layout.widths$left.padding
#	[1] 1
#	
#	$layout.widths$right.padding
#	[1] 1
#	
#	$layout.widths$axis.ylab.padding
#	[1] 1
#	
#	$layout.widths$axis.right
#	[1] 1
#	
#	$layout.widths$axis.left
#	[1] 0.9
#	
#	
#	$layout.heights
#	$layout.heights$bottom.padding
#	[1] 1
#	
#	$layout.heights$top.padding
#	[1] 1
#	
#	$layout.heights$axis.xlab.padding
#	[1] 0.8
#	
#	$layout.heights$axis.top
#	[1] 0.8
#	
#	$layout.heights$axis.bottom
#	[1] 0.9
#	
#	
#	$panelLayout
#	$panelLayout$layout
#	numeric(0)
#	
#	
#	$panelMisc
#	$panelMisc$as.table
#	[1] TRUE
#	
#	
#	$legend
#	$legend$position
#	[1] "right"
#	
#	$legend$cex
#	[1] 0.7
#	
#	$legend$maxTitleLength
#	[1] Inf
#	
#	
#	$strip
#	$strip$stripfun
#	function(..., var.name)
#	{
#		if(is.null(var.name)) strip.names = c(FALSE, TRUE) else strip.names = c(TRUE, TRUE)
#		strip.default(..., var.name = var.name, strip.names = strip.names)
#	}
#	
#	$strip$strip.bg
#	[1] "#E6E6E6" "#C3C3C3" "#969696" "#4C4C4C"
#	
	
	
}

#' This routine runs the RNMGraphics unit test suite, and produces test graphs (but external tools are needed to validate
#' them against accepted "master" graphs.  Checks are performed to ensure that all expected graphs are actually generated.
#' @param protocolFile [C,1] Name of the file to write the html unit test report to
#' @param internalTestPath [C,1] Path to the unit test scripts distributed with the package
#' @param diagnosticFile [C,1] Name of a file to output additional diagnostic information to.  This 
#' will hold information about when various other packages (which are dependencies for ERTK) were built
#' @title Run RNMGraphics package unit tests and generate graphs for comparison
#' @return Object returned by the runTestSuite function
#' @keywords programming,debugging
#' @author fgochez
#' @export

runRNMGraphicsTests <- function(protocolFile = "RNMGraphicsTests.html", 
		internalTestPath = system.file(package = "RNMGraphics", "unittests"), 
		diagnosticFile = "testenvinfo.txt", 
		genComparisonTable = TRUE)
{
	require(RUnit)
	if(!exists(".RNMGraphicsTestEnv"))
		setupTestEnv()
	# set up a global test environment
	if(!file.exists(.RNMGraphicsTestEnv$testOutputPath))
		dir.create(.RNMGraphicsTestEnv$testOutputPath)
	# write information about the environment in which these tests have been executed
	
	RNMGraphicsVer <- packageDescription("RNMGraphics", fields = "Built")
	RNMImportVer <- packageDescription("RNMImport", fields = "Built")
	
	cat( "RNMGraphics: ", RNMGraphicsVer, "\n" , file = diagnosticFile )
	cat( "RNMImport: ", RNMImportVer, "\n" , file = diagnosticFile, append = TRUE )
	
	internalTestSuite <- defineTestSuite( "RNMGraphicsInternalTestSuite", dirs = internalTestPath )
	
	res <- runTestSuite(internalTestSuite)
	
	# generate HTML report of unit test result
	
	printHTMLProtocol(res, protocolFile )
	
	# generate a table for running automated comparison tool
	# currently works for windows test cases ONLY
	
	compTables <- vector(mode = "list", length = nrow(.RNMGraphicsTestEnv$manifest))
	manifest <- .RNMGraphicsTestEnv$manifest
	
	# generate comparison tables for individual plot types, to be bound into a single data.frame at the end
	
	for(i in 1:length(compTables))
	{
		baseFiles <- paste( rownames(manifest)[i], seq(from = 1, to = manifest[i,"amount"]), sep = "", ".", .RNMGraphicsTestEnv$imgExtension )
		targetFiles <- baseFiles
		
		compTables[[i]] <- data.frame( filePath1 = "./graphtests/", filePath2 = "./expected/", 
				file1 = baseFiles, file2 = targetFiles, outFile = paste("./graphtests/_", baseFiles, sep = "") )
	}
	
	compTable <- do.call(rbind, compTables)
	write.csv("testcases.csv", x = compTable, quote = FALSE, row.names = FALSE)
	
	res
	
}
