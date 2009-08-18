\name{nmBoxPlot}
\alias{nmBoxPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NONMEM box plot }
\description{
  Creates a boxplot of continuous variables against factor variables
}
\usage{
nmBoxPlot(obj, contVar, factVar, bVars = NULL, iVar = "ID", titles = "", xLabs = NULL, xRotAngle = 0, yLabs = NULL, overlaid = FALSE, contVarOnX = FALSE, layout = NULL, maxPanels = NULL, maxTLevels = Inf, yAxisRelations = c("same", "free", "sliced"), factBin = Inf, problemNum = 1, subProblems = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ * }
  \item{contVar}{ Character vector or comma-seperated list of continuous variables from which boxplots are created }
  \item{factVar}{ Character vector or comma-seperated list of factor variables against which boxplots are created  }
  \item{bVars}{ * }
  \item{iVar}{ * }
  \item{titles}{ * }
  \item{xLabs}{ * }
  \item{xRotAngle}{ Angle by which to rotate x-angle tick labels }
  \item{yLabs}{* }
  \item{overlaid}{ If multiple contVars are specified, should they appear side by side, or on one panel? }
  \item{contVarOnX}{ If TRUE, will flip axes so that continuous variable is on the y-axis }
  \item{layout}{ * }
  \item{maxPanels}{ * }
  \item{maxTLevels}{ * }
  \item{yAxisRelations}{ * }
  \item{factBin}{ If any of the factVars have more levels than factBin, they will be binned down to this amount }
  \item{problemNum}{ * }
  \item{subProblems}{ * }
  \item{\dots}{ Additional parameters to bwplot }
}

\value{
	multiTrellis class object containing the plot
}

\author{ Mango Solutions }

\examples{
}

\keyword{ hplot }
