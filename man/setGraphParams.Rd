\name{Graphical parameters}
\alias{setGraphParams}
\alias{getGraphParams}
\alias{getAllGraphParams}

\title{Change and retrieve graphical parameters}
\description{
  \code{setGraphParams} may be used to change graphical settings in a manner similar to the \code{lattice} package's
  function \code{trellis.par.set}, while \code{getGraphParams} is similar to \code{trellis.par.get}.
}
\usage{
setGraphParams(field, setting)
getGraphParams(field)
getAllGraphParams()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{field}{A string with the name of the field/option (e.g. loess.line) that the user wishes to change or retrieve}
  \item{setting}{A named list which contains the new settings}
}
\details{
	At the moment, the following fields exist:
  \item{plot.symbol}{The plot "points" which appear in scatter plots}
  \item{superpose.symbol}{Plot "points" which appear in scatter plots whenever a grouping variable is used}
  \item{plot.line}{The plot lines which appear in scatter plots, whenever lines are chosen to connect data rather than points}
  \item{plot.text}{Plot labels that appear in scatter plots when type "i" is used}
  \item{loess.line}{The loess smoother curve which can be added to scatter plots}
  \item{histogram}{Fill colour and outline colour of histogram rectangle}
  \item{refline}{Reference lines that can be added to many plots}
  \item{barchart}{Fill colours of polygons used by \code{nmBarchart}.}
  \item{axis.text}{Text that appears in y and x axis labels}
  \item{box.rectangle}{Rectangles in box-plots}
  \item{title.text}{Plot main title text}
  \item{grid}{Reference/background grid text}
   
}
\value{
  \code{setGraphParams} returns nothing.  \code{getGraphParams} returns a list with the configuration for the particular
  field/option chose, and \code{getAllGraphParams} returns a list (of lists) with all current configuration information.
  
}
\author{Mango Solutions <support@mango-solutions>}
\note{ Some of these settings and their implementations will almost certainly change in forthcoming releases.
In particular, the use of the function \code{trellis.par.set} for implementation will likely be eliminated.  At the moment,
these are the functions fields plot.symbol, plot.line and superpose.polygon.}

\seealso{\code{\link{trellis.par.set}}}
\examples{
	nmScatterPlot(mtcars, xVars = "wt", yVars = "mpg, disp")
	plot.symbol <- getGraphParams("plot.symbol")
	old.plot.symbol <- plot.symbol
	plot.symbol$col <- "black"; plot.symbol$pch = 1
	setGraphParams("plot.symbol", plot.symbol)
	nmScatterPlot(mtcars, xVars = "wt", yVars = "mpg, disp")
	setGraphParams("plot.symbol", old.plot.symbol)
	
}

\keyword{ environment }
