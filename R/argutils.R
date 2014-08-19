#' 
#' @param assignTo 
#' @param valsToRepeat 
#' @param length.out 
#' @title Repeat Variables
#' @return NULL
#' @author Mango Solutions
#' @keywords utility
#' @noRd

repeatVars <- function(assignTo, valsToRepeat, length.out = 1)
{
	stopifnot(length(assignTo) == length(valsToRepeat))
	for(i in seq_along(assignTo)) assign(assignTo[i], rep(valsToRepeat[[i]], length.out = length.out), parent.frame(1))
}

# this function does all processing of trellis variables.  It partitions those having a more than "maxLevels",
# coerces others to factors, and returns a list of the processed data frame

processTrellis <- function(df, columns, excludeClasses, maxLevels, postFix = NULL, exemptColumns = "ID")
{
	# TODO: UNITTEST
	if (is.character(maxLevels)) {  # Character parsing
		nowWarn <- options()$warn
		options(warn = 2)
		maxLevels <- try(as.numeric(CSLtoVector(maxLevels)), silent = TRUE)
		if (class(maxLevels) == "try-error") RNMGraphicsStop("Could not parse maximum levels input")
		options(warn = nowWarn)
	}
	columns <- intersect(columns, names(df))
	outputNames <- columns
	outDf <- df
	for(n in seq_along(columns))
	{
		column <- columns[[n]]
		# if numeric and has too many levels, bin it
		if(is.numeric(df[[column]]) && !(column %in% exemptColumns)) {
			if (!is.null(postFix)) newVarName <- paste(column, postFix, sep = ".") else newVarName <- column
			if (length(maxLevels) == 1) {
				if (length(unique(df[[column]])) > maxLevels ) {
					outDf <- addDerivedCategorical(outDf, column, newVarName, breaks = maxLevels, binType = "counts")
				}
			}
			else {
				outDf <- addDerivedCategorical(outDf, column, newVarName, breaks = maxLevels, binType = "explicitcuts")
			}
			# reference the binned variable
			outputNames[n] <-  newVarName 
		}
	}
	return(list(data = outDf, columns = outputNames))
}

coerceToFactors <- function(df, columns, ordered = FALSE, excludeClasses = "shingle")
{
	columns <- intersect(columns, names(df))
	for(n in columns) 
	{
		if(!inherits(df[[n]], excludeClasses))
			df[[n]] <- factor(df[[n]], ordered = ordered)
	}
	df
}

# need to replace the RNMImport version

CSLtoVector <- function(txt, sep =",", removeBlank = TRUE, removeEmpty = FALSE) 
{
	sep <- if(removeBlank) sprintf("[[:space:]]*(%s)[[:space:]]*", sep ) else sep 
	outTxt <- unlist(strsplit(txt, split = sep))
	if(removeEmpty)
		outTxt <- outTxt[outTxt != ""]
	if(any(outTxt == "<NONE>")) {
		if(length(outTxt) == 1) {
			return(character(0))
		}
		outTxt <- outTxt[outTxt != "<NONE>"]
	}
	outTxt
}

# utility function that maps RNMGraphics settings to trellis settings 
# assumes that settings has all of the settings
mapTopar.settings <- function(settings = getAllGraphParams())
{
	# TODO: this may need to vary by plots
	with(settings, 
			list(plot.symbol =plot.symbol, superpose.symbol = superpose.symbol,
					par.xlab.text = axis.text, par.ylab.text = axis.text,
					par.main.text = title.text, plot.line = plot.line,
					add.line = refline, strip.background = strip.bg, 
					layout.widths = layout.widths, layout.heights = layout.heights, 
					superpose.polygon = barchart, box.rectangle = boxplot[c("alpha","col","fill","lty","lwd")],
					box.umbrella = list(col = boxplot$umb.col, lty = boxplot$umb.lty, 
							lwd = boxplot$umb.lwd))) 
}