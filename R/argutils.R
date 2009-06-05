# $Rev$
# $LastChangedDate$

#' 
#' @param assignTo 
#' @param valsToRepeat 
#' @param length.out 
#' @title
#' @return 
#' @author fgochez
#' @keywords

repeatVars <- function(assignTo, valsToRepeat, length.out = 1)
{
	stopifnot(length(assignTo) == length(valsToRepeat))
	for(i in seq_along(assignTo)) assign(assignTo[i], rep(valsToRepeat[[i]], length.out = length.out), parent.frame(1))
}

coerceToFactors <- function(df, columns)
{
	columns <- intersect(columns, names(df))
	for(n in columns) df[[n]] <- as.factor(df[[n]])
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