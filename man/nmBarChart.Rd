\name{nmBarChart}
\alias{nmBarChart}
\title{NONMEM categorical barchart}
\usage{
nmBarChart(obj, xVars, yVars, bVars = NULL, xLab = NULL, xRotAngle = 0,
  yLab = NULL, titles = NULL, addLegend = TRUE, xBin = Inf,
  problemNum = 1, subProblems = 1, yAxisScaleRelations = c("same", "free",
  "sliced"), xAxisScaleRelations = c("same", "free", "sliced"), ...)
}
\arguments{
  \item{obj}{NMProblem or data.frame object}

  \item{xVars}{Character vector specifying x-axis variable.
  Only one may be specified at the moment.}

  \item{yVars}{Character vector specifying y-axis variable.
  Only one may be specified at the moment.}

  \item{bVars}{Trellis variables, specified as characters
  (or \code{NULL}, which is the default).}

  \item{xLab}{x-axis label}

  \item{yLab}{y-axis label}

  \item{titles}{Main plot title}

  \item{xRotAngle}{Angle by which to rotate x-angle tick
  labels.}

  \item{xBin}{X-axis binning: Either a single value for
  range binning or explicit cut points.}

  \item{addLegend}{Logical. Should we add a legend to the
  plot?}

  \item{problemNum}{The problem required for a \code{NMRun}
  object.}

  \item{subProblems}{The sub problem of a run with
  simulations.}

  \item{yAxisScaleRelations}{Y-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{xAxisScaleRelations}{X-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{...}{additional parameters passed to the barchart
  function}
}
\value{
Multitrellis class object
}
\description{
Generates a categorical barchart of a set of categorical
variables against another one.
}
\examples{
\dontrun{
nmBarChart(mtcars, xVars = "cyl", yVars = "gear", bVars = "vs")
}
}
\author{
Mango Solutions
}
\keyword{hplot}

