# SVN revision: $Rev$
# Date of last change: $LastChangedDate$
# Last changed by: $LastChangedBy$
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



#' Set all graphical style options
#' @param settings A full list of graphical parameters.  See the documentation for RNMGraphics.
#' @title Set all graphical parameters
#' @return None
#' @author fgochez
#' @keywords utilities
#' @export

setAllGraphParams <- function(settings)
{
	for(i in seq_along(settings)) {
		setGraphParams(names(settings)[i], settings[[i]])
	}
}

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

