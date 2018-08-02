\name{nmDotPlot}
\alias{nmDotPlot}
\title{NONMEM dot plot}
\usage{
nmDotPlot(obj, factVar, contVar, bVars = NULL, iVar = "ID", gVar = "NULL",
  title = NULL, xLabs = NULL, yLabs = NULL, layout = NULL,
  maxPanels = numeric(0), addLegend = TRUE, maxTLevels = Inf,
  maxFactPerPanel = Inf, problemNum = 1, subProblems = 1,
  xAxisScaleRelations = c("same", "free", "sliced"),
  yAxisScaleRelations = c("same", "free", "sliced"),
  xAxisPlotStyle = c("data", "cont", "cat"), ...)
}
\arguments{
  \item{obj}{An object of class \code{NMRun},
  \code{NMProblem}, or \code{data.frame}. The object from
  which data will be plotted.}

  \item{factVar}{Character variable representing the factor
  variable.}

  \item{contVar}{Character vector or comma separated string
  specifying continuous variables.}

  \item{bVars}{Trellis variables, specified as characters
  (or \code{NULL}, which is the default).}

  \item{iVar}{Subject identifier variable (single string or
  NULL).}

  \item{gVar}{Grouping variable}

  \item{title}{Plot main title}

  \item{xLabs}{X-axis label}

  \item{yLabs}{Y-axis label}

  \item{layout}{Numeric vector giving the number of
  columns, rows and pages in a multipanel display.  is
  overwritten by maxPanels.}

  \item{maxPanels}{Maximum number of panels that should
  appear on each page of a graph.}

  \item{addLegend}{Logical flag.  Should legends be added?}

  \item{maxTLevels}{If a single numeric (or string), the
  maximum number of levels that a "by" variable can have
  before it is binned.  If a character vector or a vector
  of length greater than one, the explicit breakpoints.}

  \item{problemNum}{The problem required for a \code{NMRun}
  object.}

  \item{subProblems}{The sub problem of a run with
  simulations.}

  \item{maxFactPerPanel}{Max fact per panel.}

  \item{xAxisPlotStyle}{"data" or "cont" or "cat".}

  \item{yAxisScaleRelations}{Y-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{xAxisScaleRelations}{X-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{...}{Additional parameters to \code{dotplot}.}
}
\value{
Multitrellis class object containing the plot.
}
\description{
Create a custom dotplot of one or more continuous variables
against a categorical variable
}
\examples{
\dontrun{
 Theoph.df <- as.data.frame(Theoph)
 nmDotPlot( Theoph.df, factVar = "Subject", contVar = "Dose" )
}
}
\author{
Mango Solutions
}
\keyword{hplot}

