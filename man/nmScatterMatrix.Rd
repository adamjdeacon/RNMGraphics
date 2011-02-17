\name{nmScatterMatrix}
\alias{nmScatterMatrix}
\alias{nmScatterMatrix,data.frame-method}
\alias{nmScatterMatrix,NMProblem-method}
\alias{nmScatterMatrix,NMRun-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NONMEM Scatter plot Matrix }
\description{
  Creates scatter plot matrices customised for NONMEM data sets.
}
\usage{
nmScatterMatrix(obj, vars, bVars = NULL, iVar = "ID",  addLoess = FALSE, title ="", layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ An object of class \code{NMRun}, \code{NMProblem}, or \code{data.frame}. The object from which data will be plotted. }
  \item{vars}{ Character vector or comma seperated list of variable names to plot }
  \item{bVars}{ character vector or comma-seperated list of trellis variables }
  \item{iVar}{ Subject identifier variable. }
  \item{addLoess}{ Add a loess smoother? }
  \item{title}{ Main title. }
  \item{layout}{ Numeric vector giving the number of columns, rows and pages in a multipanel display. }
  \item{maxPanels}{ Maximum number of panels that should appear on each page of a graph.  Overrides \code{layout} }
  \item{maxTLevels}{ If a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned.
  					 If a character vector or a vector of length greater than one, the explicit breakpoints. }
  \item{problemNum}{ The problem required for a \code{NMRun} object. }
  \item{subProblems}{ The sub problem of a run with simulations. }
  \item{\dots}{ Additional parameters to \code{splom}. }
}

\value{
	Multitrellis class object containing the plot.
}

\author{ Mango Solutions <support@mango-solutions> }

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	nmScatterMatrix(x, "DV,PRED,IPRED", addLoess = TRUE)
}

\keyword{ hplot }


