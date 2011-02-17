\name{panel layout}
\alias{panelLayout<-}

\title{Assign panel layout }

\description{
  \code{panelLayout<-} assigns a layout to the panels of the plot held in the object \code{x}
}
\usage{
	panelLayout(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class \code{multiTrellis} }
  \item{value}{ A length 2 vector (number of rows and columns in the layout) }
}

\value{
	Updates the object "in place"
}

\author{ Mango Solutions <support@mango-solutions> }

\examples{
	x <- nmScatterPlot(mtcars, "mpg", "wt", bVars = "gear,carb")
	panelLayout(x) <- c(2,2)
	print(x)
	
}

\keyword{ hplot }



