\name{nmScatterMatrix}
\alias{nmScatterMatrix}
\title{NONMEM scatter-plot matrix}
\usage{
nmScatterMatrix(obj, vars, bVars = NULL, iVar = "ID", addLoess = FALSE,
  title = "", layout = NULL, maxPanels = NULL, maxTLevels = Inf,
  problemNum = 1, subProblems = 1, ...)
}
\arguments{
  \item{obj}{object of class NMProblem, NMRun or
  data.frame}

  \item{vars}{Character vector or comma seperated list of
  variable names to plot}

  \item{bVars}{character vector or comma-seperated list of
  trellis variables}

  \item{iVar}{Identifier variable}

  \item{addLoess}{Logical flag. Should a loess smoother
  curve be added to the scatter-plots?}

  \item{title}{Plot's main title}

  \item{layout}{Numeric vector giving the number of
  columns, rows and pages in a multipanel display.}

  \item{maxPanels}{Maximum number of panels that should
  appear on each page of a graph.  Overrides \code{layout}}

  \item{maxTLevels}{If a single numeric (or string), the
  maximum number of levels that a "by" variable can have
  before it is binned.  If a character vector or a vector
  of length greater than one, the explicit breakpoints.}

  \item{problemNum}{The problem required for a \code{NMRun}
  object.}

  \item{subProblems}{The sub problem of a run with
  simulations.}

  \item{...}{Additional parameters to \code{splom}.}
}
\value{
An object of class multiTrellis
}
\description{
Generates a scatterplot matrix of a set of variables from a
PK/PD dataset
}
\author{
Mango Solutions
}
\keyword{hplot}

