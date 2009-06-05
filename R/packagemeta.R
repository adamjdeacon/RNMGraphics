# $Rev$
# $LastChangedDate$


getAllGraphParams <- function()
{
	.RNMGraphicsEnv$graphPars
}

#' \code{setGraphParams} may be used to change graphical settings in a manner similar to the \code{lattice} package's
#' function \code{trellis.par.set}
#' @title Change and retrieve graphical parameters
#' @param field A string with the name of the field/option (e.g. loess.line) that the user wishes to change or retrieve
#' @param setting A named list which contains the new settings
#' @return None
#' @author fgochez
#' @keywords environment

setGraphParams <- function(field, setting)
{
	# TODO: implement a mapping here so that the list names don't have to coincide with lattice settings
	
	RNMGraphicsStopifnot(field %in% names(.RNMGraphicsEnv$graphPars), msg = "Setting is not in the available graphics options")
	
		# allow the user to replace a subset of the settings
	x <- getGraphParams(field)
	changedSettings <- intersect(names(setting), names(x))
	x[changedSettings] <- setting[changedSettings]
	.RNMGraphicsEnv$graphPars[[field]] <- x
	
	
}

#' similar to \code{trellis.par.get}.  Retrieves a named list of settings associated to a 
#' particular graphical setting
#' @title Retrieve graphical parameters
#' @param field A string with the name of the field/option (e.g. loess.line) that the user wishes to change or retrieve 
#' @return A named list 
#' @author fgochez
#' @keywords environment

getGraphParams <- function(field)
{
	RNMGraphicsStopifnot(field %in% names(.RNMGraphicsEnv$graphPars), msg = "Setting is not in the available graphics options")
	.RNMGraphicsEnv$graphPars[[field]]
}

getStripFun <- function()
{
	# do.call(strip.custom, getGraphParams("strip"))
	.RNMGraphicsEnv$graphPars[["strip"]]$stripfun
}

# gets a subset of the graphical settings that map directly to lattice settings.

mapTopar.settings <- function()
{
	with(getAllGraphParams(), list(
					plot.symbol =plot.symbol, superpose.symbol = superpose.symbol,
					par.xlab.text = axis.text, par.ylab.text = axis.text,
					par.main.text = title.text, plot.line = plot.line,
					add.line = refline, strip.background = strip.bg, 
					layout.widths = layout.widths, layout.heights = layout.heights)) 
}