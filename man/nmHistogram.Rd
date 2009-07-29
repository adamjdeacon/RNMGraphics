\name{nmHistogram}
\alias{nmHistogram}

\title{ NONMEM data histogram }
\description{
  creates histograms of one or more NONMEM variables
}
\usage{
nmHistogram(obj, vars, bVars = NULL, iVar = "ID", refLine = "none", type = "percent", addDensity = FALSE, titles = "", xLabs, extraSubset, addGrid = TRUE, nint = 12, breaks, layout = NULL, maxPanels = NULL, maxTLevels = Inf, problemNum = 1, subProblems = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ * }
  \item{vars}{ Variables from which to generate a histogram (character vector or comma seperate string of names) }
  \item{bVars}{ "Trellis” variables on which to split data.}
  \item{iVar}{ Subject identifier variable }
  \item{refLine}{ Controls addition of a reference line to the histogram(s).  Choices are "none", "mean" or "median". }
  \item{type}{ Determines the style of y-axis that is used for the plot (percentages, frequencies, or proportions) }
  \item{addDensity}{ Logical flag.  Should a density estimate be plotted? Only relevant for type = "density" }
  \item{titles}{Plot titles }
  \item{xLabs}{ x axis labels}
  \item{extraSubset}{ Currently unused }
  \item{addGrid}{ Currently unused }
  \item{nint}{ Number of intervals for the creation of X axis bars.  It functions identically to the nint parameter of the histogram function from the lattice package }
  \item{breaks}{Control the calculation of breakpoints for the histogram.  It functions identically to the breaks parameter of the histogram function from the lattice package.}
  \item{layout}{ *}
  \item{maxPanels}{ * }
  \item{maxTLevels}{ * }
  \item{problemNum}{ * }
  \item{subProblems}{ * }
  \item{\dots}{ Additional parameters to histogram function}
}
\details{
 
}
\value{
	multiTrellis class object holding the plot
}
\author{Mango Solutions}

\examples{

}

\keyword{ hplot }
