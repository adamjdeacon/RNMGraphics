\name{nmACPlot}
\alias{nmACPlot}
\alias{nmACPlot,data.frame-method}
\alias{nmACPlot,NMProblem-method}
\alias{nmACPlot,NMRun-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NONMEM Autocorrelation Plot }
\description{
  Plots a given variable in a  NONMEM data set against itself "lagged" by one time step.
}
\usage{
nmACPlot(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL, gVars = NULL, titles = "", xLabs = NULL, yLabs = NULL, extraSubset = NULL, addGrid = TRUE, problemNum = 1, subProblems = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ An object of class \code{NMRun}, \code{NMProblem}, or \code{data.frame}. The object from which data will be plotted. }
  \item{var}{ Name of the variable to be plotted }
  \item{tVar}{ TName of the "time" variable.}
  \item{iVar}{ Subject identifier variable. }
  \item{bVars}{ Trellis variables, specified as characters (or \code{NULL}, which is the default). }
  \item{gVars}{ “Grouping” variable – used to group points by colour, for legends and so on. }
  \item{titles}{ Main title. }
  \item{xLabs}{ X-axis label. }
  \item{yLabs}{ Y-axis label. }
  \item{extraSubset}{ Not used at the moment. }
  \item{addGrid}{ Should a grid be added? }
  \item{problemNum}{ The run required for a \code{NMRun} object. }
  \item{subProblems}{ The sub problem of a run with simulations. }
  \item{\dots}{ Additional parameters to \code{nmScatterPlot}. }
}

\value{
	Multitrellis class object containing the plot.
}

\author{ Mango Solutions <support@mango-solutions> }

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	nmACPlot(x, "RES", gVars = "SEX", bVars = "RACE", addLegend = TRUE)
}

\keyword{ hplot }



