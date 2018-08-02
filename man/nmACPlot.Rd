\name{nmACPlot}
\alias{nmACPlot}
\title{NONMEM autocorrelation plot}
\usage{
nmACPlot(obj, var, tVar = "TIME", iVar = "ID", bVars = NULL,
  gVars = NULL, titles = "", xLabs = NULL, yLabs = NULL,
  extraSubset = NULL, addGrid = TRUE, problemNum = 1, subProblems = 1,
  ...)
}
\arguments{
  \item{obj}{NMProblem or data.frame object}

  \item{var}{Name of the variable to be plotted}

  \item{tVar}{Name of the "time" variable}

  \item{iVar}{Identifier variable}

  \item{bVars}{"by" variables}

  \item{gVars}{grouping variable}

  \item{titles}{Main title}

  \item{xLabs}{x-axis label}

  \item{yLabs}{y-axis label}

  \item{extraSubset}{Not used at the moment}

  \item{addGrid}{logical flag.  Should a grid be added?}

  \item{problemNum}{The problem required for a \code{NMRun}
  object.}

  \item{subProblems}{The sub problem of a run with
  simulations.}

  \item{...}{Extra parameters passed to nmScatterPlot}
}
\value{
Obejct of class multiTrellis
}
\description{
nmACPlot plots a given variable in a data set against
itself "lagged" by one time step.
}
\author{
Mango Solutions
}
\keyword{hplot}

