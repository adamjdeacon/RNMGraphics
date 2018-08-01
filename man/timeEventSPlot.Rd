\name{timeEventSPlot}
\alias{timeEventSPlot}
\title{NONMEM Time to Event Scatter Plot}
\usage{
timeEventSPlot(obj, concVar = "DV", tVar = "TIME", doseVar = "AMT",
  evtVar = "EVID", iVar = "ID", subjectNum = NULL, title = NULL,
  xLab = NULL, yLab = NULL, layout = NULL, maxPanels = NULL,
  problemNum = 1, subProblems = 1, xAxisScaleRelations = c("same", "free",
  "sliced"), yAxisScaleRelations = c("same", "free", "sliced"),
  expX = FALSE, expY = FALSE, xRotAngle = 0, ...)
}
\arguments{
  \item{obj}{An object of class \code{NMRun},
  \code{NMProblem}, or \code{data.frame}. The object from
  which data will be plotted.}

  \item{concVar}{Dependent variable.}

  \item{tVar}{Time variable.}

  \item{doseVar}{Dosing amount variable.}

  \item{evtVar}{Event identifier variable.}

  \item{iVar}{Subject identifier variable.}

  \item{subjectNum}{A vector of subject numbers. Otherwise,
  all subject numbers are plotted.}

  \item{title}{Main title}

  \item{xLab}{X-axis label}

  \item{yLab}{Y-axis label}

  \item{layout}{Numeric vector giving the number of
  columns, rows and pages in a multipanel display}

  \item{maxPanels}{Maximum number of panels to display on
  each page}

  \item{problemNum}{The problem required for a \code{NMRun}
  object}

  \item{subProblems}{The sub problem of a run with
  simulations}

  \item{yAxisScaleRelations}{Y-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{xAxisScaleRelations}{X-axis scale relations when
  panels are displayed. One of \code{"same"}, \code{"free"}
  or \code{"sliced"}.}

  \item{expX}{Logical flag.  Exponentiate x-axis variable?}

  \item{expY}{Logical flag.  Exponentiate the y-axis
  variables?}

  \item{xRotAngle}{Single numeric.  Angle by which to
  rotate}

  \item{...}{Additional parameters to xyplot}
}
\value{
Multitrellis class object containing the plot.
}
\description{
Generate scatter plot of concentration/DV against time for
each individual in a NONMEM object.  These will be line
plots, with vertical reference lines at dosing times
}
\examples{
\dontrun{
Theoph2 <- as.data.frame(transform(Theoph, Evt = as.numeric((Time == 0)) ))
timeEventSPlot(Theoph2, tVar = "Time", concVar = "conc",
  doseVar = "Dose", evtVar = "Evt", iVar = "Subject")
}
}
\author{
Mango Solutions
}
\keyword{hplot}

