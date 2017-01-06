

setupTestEnv <- function(testDataPath = system.file("unittests", "testdata" ,package = "RNMGraphics"), graphOutputPath = "graphtests")
{
	NUMSUPERPOSELEVELS <- 7
	
	.RNMGraphicsTestEnv <<- new.env()
	# path where the test data should be found
	.RNMGraphicsTestEnv$testDataPath <- testDataPath
	.RNMGraphicsTestEnv$testOutputPath <- graphOutputPath
	.RNMGraphicsTestEnv$testDevice <- "jpeg"
	.RNMGraphicsTestEnv$imgExtension <- "jpg"
	.RNMGraphicsTestEnv$deviceSettings <- list(width = 400, height = 400, quality = 75)
	
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
	
	newStyles$legend <- list(position = "top", cex = 1, maxTitleLength  = 7)
	
	.RNMGraphicsTestEnv$newStyles <- newStyles
	
	
}

setupExpectedMD5 <- function(testDataPath = system.file("unittests", "testdata" ,package = "RNMGraphics"), branchname = NULL) {

	if (is.null(branchname)) {
		ExpectedMD5File <- paste("ExpectedMD5.csv", sep = "")
	} else {
		ExpectedMD5File <- paste("ExpectedMD5_", branchname, "_", R.Version()$major, ".", R.Version()$minor, ".csv", sep = "")
	}
	.ExpectedMD5 <- read.csv(file.path( testDataPath, ExpectedMD5File), stringsAsFactors = FALSE)
	if (.Platform$OS.type == "windows") {
		.RNMGraphicsTestEnv$ExpectedMD5 <- .ExpectedMD5$WindowsMD5
	} else {
		.RNMGraphicsTestEnv$ExpectedMD5 <- .ExpectedMD5$LinuxMD5
	}
	names(.RNMGraphicsTestEnv$ExpectedMD5) <- .ExpectedMD5$Graph
}

getMD5 <- function(e){
	f = paste(tempfile(), ".jpg", sep = "")
	do.call(jpeg, c(list(filename = f), .RNMGraphicsTestEnv$deviceSettings))
	force(e)
	dev.off()
	unname(tools::md5sum(f))
}

#' Run unit tests.
#'
#' Run the unit tests by RUnit package, and generate a html or text report. 
#' @title Run unit tests.
#' @param TestPath Path of the folder which contains the test scripts.
#' @param ExcludeFolders The folders are not tested.
#' @param TestResult Name of the report file.
#' @param ResultsType 'html' or 'text'.
#' @return The results of function \code{\link{[RUnit]runTestSuite}}. 
#' @author Mango Solutions
#' @keywords debugging
#' @examples \dontrun{
#' x <- runRNMGraphicsTests(TestResult = "runRNMGraphicsTests_tests")
#' summary(x)
#' }
#'
#' @noRd 

runRNMGraphicsTests <- function(TestPath = system.file(package="RNMGraphics", "unittests"), 
		ExcludeFolders = NULL, TestResult = NULL, ResultsType = c("html", "text"))
{
	if(!require("RUnit", quietly = TRUE)) stop("There is no 'RUnit' package!")
	TestPath <- normalizePath(TestPath, winslash = "/", mustWork = TRUE)
	ResultsType <- match.arg(ResultsType)
	
	if(!exists(".RNMGraphicsTestEnv")) setupTestEnv()
	.RNMGraphicsTestEnv$testDataPath <- file.path(TestPath, "testdata")
	setupExpectedMD5(testDataPath = file.path(TestPath, "testdata"))
	
	# There is no 'recursive' argument in 'list.dirs' under R 2.13.1
	TestFolders <- list.dirs(TestPath, full.names = TRUE)
	TestFiles <- list.files(TestPath, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
	TestFolders <- intersect(TestFolders, TestFiles)
	TestFolders <- TestFolders[!basename(TestFolders) %in% ExcludeFolders]
	if (length(TestFolders) > 0) {
		TestSuite <- list()
		for (i in seq_along(TestFolders)) {
			TestSuiteName <- paste("RNMGraphics Tests - ", basename(TestFolders)[i], sep = "")
			TestSuite[[i]] <- defineTestSuite(TestSuiteName, dirs = TestFolders[i], testFileRegexp = "^runit\\..+\\.[rR]$") 
		}
	} else {
		TestSuite <- defineTestSuite("RNMGraphics Tests", dirs = TestPath, testFileRegexp = "^runit\\..+\\.[rR]$")
	}
	
	OUT <- runTestSuite(TestSuite)
	if(!is.null(TestResult)) {
		TestResult <- paste(gsub(paste("\\.", ResultsType, sep = ""), "", 
						TestResult, ignore.case = TRUE), ResultsType, sep = ".")
		if (ResultsType == "html") printHTMLProtocol(OUT, fileName = TestResult)
		if (ResultsType == "text") printTextProtocol(OUT, fileName = TestResult)
	} 
	
	return(OUT)
	
}
