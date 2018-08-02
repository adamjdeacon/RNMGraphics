\name{panelLayout<-}
\alias{panelLayout<-}
\title{Assign panel layout}
\usage{
panelLayout(x) <- value
}
\arguments{
  \item{x}{\code{panelLayout<-} assigns a layout to the
  panels of the plot held in the object \code{x}}

  \item{value}{A length 2 vector (number of rows and
  columns in the layout)}
}
\value{
Updates the object "in place"
}
\description{
\code{panelLayout<-} assigns a layout to the panels of the
plot held in the object \code{x}.
}
\examples{
\dontrun{
x <- nmScatterPlot(mtcars, "mpg", "wt", bVars = "gear,carb")
panelLayout(x) <- c(2,2)
print(x)
}
}
\author{
Mango Solutions
}
\keyword{hplot}

