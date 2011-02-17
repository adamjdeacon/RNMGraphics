\name{nmQQNorm}
\alias{nmQQNorm}
\title{NONMEM qqplot}
\usage{nmQQNorm(obj, vars, bVars, iVar="ID", titles="", xLabs="normal", yLabs, addGrid=TRUE, qqLine=TRUE, yAxisScaleRelations=c("same", "free", "sliced"), layout,
    maxPanels, maxTLevels=Inf, problemNum=1, subProblems=1, xAxisScaleRelations=c("same", "free", "sliced"), ...)
}
\description{Generates a qq-plot (for the normal distribution) from one or more NONMEM variables}
\value{Multi-trellis class object containing the plot}
\author{Mango Solutions}
\keyword{hplot}
\arguments{\item{obj}{NMRun, NMProblem, or data.frame object}
\item{vars}{Vector or comma-separated list of variables to plot}
\item{bVars}{Vector or comma-separated list of "by" variables}
\item{titles}{Main title}
\item{xLabs}{x-axis label}
\item{yLabs}{y-axis label}
\item{addGrid}{unused}
\item{qqLine}{logical flag.  Should a reference line be added?}
\item{yAxisScaleRelations}{One of "same" "free" "sliced". How panel y axes are scaled in relation to each other}
\item{layout}{Numeric vector giving the number of columns, rows and pages in a multipanel display.}
\item{maxPanels}{Maximum number of panels that should appear on each page of a graph.}
\item{maxTLevels}{If a single numeric (or string), the maximum number of levels that a "by" variable can have before it is binned.
If a character vector or a vector of length greater than one, the explicit breakpoints.}
\item{problemNum}{The problem required for a \code{NMRun} object.}
\item{subProblems}{The sub problem of a run with simulations.}
\item{yAxisScaleRelations}{One of "same" "free" "sliced". How panel x axes are scaled in relation to each other}
\item{...}{additional parameters to pass to qqmath}
}
\examples{nmQQNorm(ChickWeight.df,"weight", bVar =  "Diet", xAxisScaleRelations="free")}

