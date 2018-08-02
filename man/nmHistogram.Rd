\name{nmHistogram}
\alias{nmHistogram}
\title{NONMEM data histogram}
\usage{
nmHistogram(obj, vars, bVars = NULL, iVar = "ID", refLine = "none",
  type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset,
  addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL,
  xRotAngle = 0, maxTLevels = Inf, problemNum = 1, subProblems = 1,
  xAxisScaleRelations = c("same", "free", "sliced"),
  yAxisScaleRelations = c("same", "free", "sliced"), ...)
}
\arguments{
  \item{obj}{The object from which data will be plotted
  (NMRun, NMProblem or data.frame)}

  \item{vars}{Variables from which to generate a histogram
  (character vector or comma seperate string of names)}

  \item{bVars}{"Trellis" variables on which to split data.}

  \item{iVar}{Subject identifier variable}

  \item{refLine}{Controls addition of a reference line to
  the histogram(s).  Choices are "none", "mean" or
  "median".}

  \item{type}{Determines the style of y-axis that is used
  for the plot (percentages, frequencies, or proportions)}

  \item{addDensity}{Logical flag.  Should a density
  estimate be plotted? Only relevant for type = "density"}

  \item{titles}{Plot title}

  \item{xLabs}{x axis label}

  \item{xRotAngle}{Angle by which to rotate the x-axis tick
  marks}

  \item{extraSubset}{Currently unused}

  \item{addGrid}{Currently unused}

  \item{nint}{Number of intervals for the creation of X
  axis bars.  It functions identically to the nint
  parameter of the histogram function from the lattice
  package}

  \item{breaks}{Control the calculation of breakpoints for
  the histogram.  It functions identically to the breaks
  parameter of the histogram function from the lattice
  package.}

  \item{layout}{A length 2 vector which is passed in as the
  layout parameter to xyplot}

  \item{maxPanels}{Maximum number of panels that should
  appear on each page of a graph.}

  \item{maxTLevels}{f a single numeric (or string), the
  maximum number of levels that a "by" variable can have
  before it is binned.  If a character vector or a vector
  of length greater than one, the explicit breakpoints.}

  \item{problemNum}{Number of the problem (applicable to
  NMRun class only)}

  \item{subProblems}{Number of the simulation subproblems
  to use (applicable to the NMSim* classes obly)}

  \item{yAxisScaleRelations}{Y-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{xAxisScaleRelations}{X-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{...}{Additional parameters passed to histogram}
}
\value{
An object of class multiTrellis
}
\description{
creates histograms of one or more NONMEM variables
}
\examples{
\dontrun{
 Theoph.df <- as.data.frame(Theoph)
 nmHistogram(Theoph.df, vars = "conc",
   title = "Theophiline concentration histogram", type = "density")
}
}
\author{
Mango Solutions
}
\keyword{hplot}

