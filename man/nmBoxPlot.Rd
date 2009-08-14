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
  \item{bVars}{ ~~Describe \code{bVars} here~~ }
  \item{iVar}{ ~~Describe \code{iVar} here~~ }
  \item{titles}{ ~~Describe \code{titles} here~~ }
  \item{xLabs}{ ~~Describe \code{xLabs} here~~ }
  \item{xRotAngle}{ ~~Describe \code{xRotAngle} here~~ }
  \item{yLabs}{ ~~Describe \code{yLabs} here~~ }
  \item{overlaid}{ ~~Describe \code{overlaid} here~~ }
  \item{contVarOnX}{ ~~Describe \code{contVarOnX} here~~ }
  \item{layout}{ ~~Describe \code{layout} here~~ }
  \item{maxPanels}{ ~~Describe \code{maxPanels} here~~ }
  \item{maxTLevels}{ ~~Describe \code{maxTLevels} here~~ }
  \item{yAxisRelations}{ ~~Describe \code{yAxisRelations} here~~ }
  \item{factBin}{ ~~Describe \code{factBin} here~~ }
  \item{problemNum}{ ~~Describe \code{problemNum} here~~ }
  \item{subProblems}{ ~~Describe \code{subProblems} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}

\author{ Mango Solutions }

\examples{
}

\keyword{ hplot }
