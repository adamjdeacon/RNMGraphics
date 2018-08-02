\name{panel functions}
\alias{panel functions}
\alias{panel.nmBoxPlot}
\alias{panel.nmHistogram}
\alias{panel.overlaidScatter}
\alias{prepanel.nmBoxPlot}
\title{panel functions}
\usage{
panel.nmBoxPlot(x, y, hLines = NULL, ...)

prepanel.nmBoxPlot(x, y, horizontal, balanced = FALSE, ...)

panel.nmHistogram(x, refLine, addDensity, graphParams, ...)

panel.overlaidScatter(x, y, groups, featuresToAdd = c(grid = FALSE, loess =
  FALSE, idLine = FALSE), subscripts = seq_along(x), type = c("p", "o", "i",
  "l", "t"), idLabels = NULL, graphParams, abLines = NULL, ...)
}
\arguments{
  \item{x}{passed straight to panel.bwplot}

  \item{y}{passed straight to panel.bwplot}

  \item{hLines}{Vector of horizontal reference lines}

  \item{...}{}

  \item{x}{passed straight to prepanel.default.bwplot}

  \item{y}{passed straight to prepanel.default.bwplot}

  \item{horizontal}{logical flag.  Passed straight to
  prepanel.default.bwplot}

  \item{balanced}{If TRUE, will "balance" the axis with the
  continuous variable on it}

  \item{...}{Additional arguments passed to
  prepanel.default.bwplot}

  \item{x}{Basic parameter passed straight to
  panel.histogram}

  \item{refLine}{"none", "mean", or "median" - where to add
  a reference line}

  \item{addDensity}{logical flag.  If TRUE, density
  estimate curve will be generated}

  \item{graphParams}{Full set (list) of RNMGraphics
  graphical parameters}

  \item{...}{}

  \item{x}{(usual)}

  \item{y}{(usual)}

  \item{subscripts}{(usual)}

  \item{featuresToAdd}{named logical vector of features to
  add (grid, loess, idLine)}

  \item{idLabels}{Identifier labels}

  \item{type}{one of "p", "i", "l", "o", "t". Note that for
  "l" (lines connected by the variable specified in
  "iVars"), "o" (lines and points), and "t" (labels
  connected by lines grouped by "iVars") lines will be
  added independently within nested groups.}

  \item{groups}{(usual)}

  \item{graphParams}{Full list of RNMGraphics graphical
  settings}

  \item{...}{additional parameters to panel.xyplot /
  panel.superpose}
}
\description{
panel functions used for different objects.
}
\details{
panel.nmBoxPlot is the nmBoxPlot custom panel function.
Mainly used to add reference lines.

prepanel.nmBoxPlot is the prepanel function for nmBoxPlot.
It is needed because in certain cases we want the axis
holding the continous to be "balanced"

panel.nmHistogram is a custom panel function for
nmHistogram. In essence it just adds a density line and a
reference line if needed.

panel.overlaidScatter is the nmScatterPlot panel function
(2 of 2) used when overlaid y axis variables are requested.
}
\examples{
\dontrun{
df1 <- data.frame(X = rep(1:10, times = 2), Y1 = c(2:11, 3:12)^0.8,
    Y2 = c(3:12, 4:13)^0.6, G = rep(letters[1:2], each = 10))
xyplot(Y1 + Y2 ~ X | G, data = df1,
    panel = RNMGraphics:::panel.overlaidScatter, type = "o", idLabels = df1$G,
    graphParams = getAllGraphParams())
df1$N <- rep(1:2, times = 5)
df1 <- df1[with(df1, order(G, N, X)), ]
xyplot(Y1 + Y2 ~ X | G, data = df1,
    panel = RNMGraphics:::panel.overlaidScatter, type = "o", idLabels = df1$G,
    graphParams = getAllGraphParams())
}
}
\author{
Mango Solutions
}

