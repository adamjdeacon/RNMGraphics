\name{nmBoxPlot}
\alias{nmBoxPlot}
\title{NONMEM box plot}
\usage{
nmBoxPlot(obj, contVar, factVar, bVars = NULL, iVar = "ID", titles = "",
  xLabs = NULL, xRotAngle = 0, yLabs = NULL, overlaid = FALSE,
  contVarOnX = FALSE, layout = NULL, maxPanels = NULL, maxTLevels = Inf,
  yAxisScaleRelations = c("same", "free", "sliced"), factBin = Inf,
  problemNum = 1, subProblems = 1, hLines = NULL,
  balancedContAxis = FALSE, medianLines = TRUE, modifyOnResidual = TRUE,
  residVars = c("WRES", "IWRE", "IWRES"), xAxisScaleRelations = c("same",
  "free", "sliced"), ...)
}
\arguments{
  \item{obj}{An object of class \code{NMRun},
  \code{NMProblem}, or \code{data.frame}. The object from
  which data will be plotted.}

  \item{contVar}{Character vector or comma-seperated list
  of continuous variables from which boxplots are created}

  \item{factVar}{Factor variable (single string)}

  \item{bVars}{Trellis variables, specified as characters
  (or \code{NULL}, which is the default).}

  \item{iVar}{ubject identifier variable.}

  \item{titles}{Main title.}

  \item{xLabs}{X-axis label.}

  \item{yLabs}{Y-axis label.}

  \item{xRotAngle}{Angle by which to rotate the x-axis tick
  marks}

  \item{maxPanels}{Maximum number of panels that should
  appear on each page of a graph.}

  \item{maxTLevels}{If a single numeric (or string), the
  maximum number of levels that a "by" variable can have
  before it is binned.  If a character vector or a vector
  of length greater than one, the explicit breakpoints.}

  \item{yAxisScaleRelations}{Axis scale relations when
  multiple y-variables exist. One of \code{"same"},
  \code{"free"} or \code{"sliced"}.}

  \item{subProblems}{The sub problem of a run with
  simulations.}

  \item{overlaid}{logical flag. Should multiple factor
  variables be plotted on a single plot, or should multiple
  subplots with different factors be generated?}

  \item{problemNum}{The problem number, required for a
  \code{NMRun} object.}

  \item{contVarOnX}{If \code{TRUE}, will flip axes so that
  the continuous variable is on the x-axis.}

  \item{layout}{Numeric vector giving the number of
  columns, rows and pages in a multipanel display.}

  \item{factBin}{Single numeric. Factor variable will be
  binned to this many levels if it has more levels than
  this value. Binning done according to "counts".}

  \item{balancedContAxis}{Single logical. If TRUE, axis
  with continuous variable will extend equally in both
  directions}

  \item{medianLines}{Single logical. If TRUE, will plot
  median lines inside boxes instead of points (pch = 19)}

  \item{modifyOnResidual}{Single logical. If TRUE, will
  force balanced continuous variable axis and a horizontal
  line at y = 0 IF there is only one continuous variable
  AND it is contained in residVars.}

  \item{hLines}{Numeric vector. If not empty (NULL or zero
  length vector), horizontal lines will be added at y =
  (all values)}

  \item{residVars}{A vector of character names to count as
  residual variables (for use with
  \code{modifyOnResidual})}

  \item{xAxisScaleRelations}{Axis scale relations when
  multiple x-variables exist. One of \code{"same"},
  \code{"free"} or \code{"sliced"}.}

  \item{...}{Additional parameters to \code{bwplot}.}
}
\value{
Multitrellis class object containing the plot.
}
\description{
Creates a boxplot of continuous variables against factor
variables
}
\examples{
\dontrun{
Theoph.df <- as.data.frame(Theoph)
nmBoxPlot(Theoph.df, contVar = "conc", factVar = "Time", factBin = 6)
}
}
\author{
Mango Solutions
}
\keyword{hplot}

