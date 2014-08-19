
.RNMGraphicsEnv <- new.env()

# TODO: refactor so that the graphics parameters are configured seperately

initializeOptions <- function()
{
	# barchart
	# TODO: add more comments explaining each setting

	scal <- seq(from = 0.3, to = 1, length.out = 7)
	superpose.polygon <- list(alpha = rep(1, 7), lty = rep(1, 7), lwd = rep(1, 7), 
			border = rep("black", 7), col = grDevices::topo.colors(8)) 
	
	# initialize colours for superpose.polygon
	# histogram
	hist <- list(alpha = 1, col = "steelblue", border = "black", lty = 1, lwd = 1, dens.col = "darkred", dens.lty = 1, dens.lwd = 1.5)
	
	superpose.symbol <- list(cex = rep(0.8, 7), 
			col =  grDevices::grey.colors(7),	pch = rep(c(1, 2, 19), length.out = 8) )
	superpose.line <- list(alpha = 1, col = c("#0080ff", "#ff00ff",  "darkgreen","#ff0000",  "orange", "#00ff00","brown")  , 
			lty = rep(1, 8), lwd = rep(1, 8))
	
	# controls padding and layout of panels
	layout.widths <- list(left.padding = 1, right.padding = 1, axis.ylab.padding = 1,
		axis.right = 1, axis.left = 0.9)
	layout.heights <- list(bottom.padding = 1, top.padding = 1, axis.xlab.padding = 0.8,
			axis.top = 0.8, axis.bottom = 0.9) 
	legend = list(position = "right", cex = 0.7, maxTitleLength = Inf)
	panelLayout = list(layout = numeric(0))
	
	.RNMGraphicsEnv$graphPars <- 
		list(superpose.symbol = superpose.symbol,
			superpose.line = superpose.line,
			plot.symbol = list(alpha = 1, cex = 0.8, col = "darkblue", fill = "black", pch = 1), 
			plot.line = list(alpha = 1, col = "darkblue", lty = 1, lwd = 1),	
			plot.text = list(alpha = 1, cex = 1, col = "black"),
			superpose.text = list(alpha = rep(1, 8), cex = rep(1, 8), col = superpose.line$col),
			loess.line = list(lwd = 1.5, col = "darkred"),
			barchart = superpose.polygon,
			histogram = hist,
			"refline" = list(alpha = 1, col = "red", lty = 1, lwd = 1.5),
			"axis.text" = list(alpha = 1, cex = 0.8, col = "black", font = 1),
			"boxplot" = list(alpha = 1, col = "red", fill = "darkred", lty = 1, lwd = 1, 
							umb.col = "blue", umb.lty = 1, umb.lwd=1 ),
			"title.text" = list(alpha = 1, cex = 1.2, col = "black", font = 2, lineheight = 1),
			grid = list(col = "lightgray", lty = 1, lwd = 1),
			strip.bg = list(col = rev(grDevices::grey.colors(4))), 
					# list(col = c("#ffe5cc", "#ccffcc", "#ccffff", "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")),
			layout.widths = layout.widths,
			layout.heights = layout.heights,
			panelLayout = panelLayout,
			panelMisc = list("as.table" = TRUE),
			legend = legend,
			strip = list(stripfun = defaultStrip)
		)
	.RNMGraphicsEnv$defaultSubset <- c("MDV != 1", "EVID == 0", "AMT <= 0") 
}

updateRNMImportSettings <- function()
{
	setVarDescription("SEX", "Gender",c("0=male,1=female"), "Covariate")
}

.onLoad <- function(libname, pkgname)
{
	if ("RNMImport" %in% .packages()) {
		
		updateRNMImportSettings()
		
		initializeOptions()
		
	} else { warning("RNMImport is not loaded") }
}