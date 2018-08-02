\docType{class}
\name{multiTrellis-class}
\alias{multiTrellis-class}
\title{Class "multiTrellis".}
\description{
A class designed to hold multiple lattice plots and their
layout on a single device page. At the moment, this is
mostly unimplemented and only a single plot is stored.
}
\section{Objects from the Class}{
  Objects of this class are typically returned by plotting
  routines in this package.  They can also be created via
  the \code{multiTrellis} function, though this is not
  meant to be done directly.
}

\section{Slots}{
  \describe{ \item{\code{plots}:}{List of plots to include
  in the multiTrellis object.  Currently only one plot is
  allowed.}

  \item{\code{layout}:}{Currently unused.  Meant to store
  the layout of the plots on a device in the future.}

  \item{\code{mainTitle}:}{Currently unused.}

  \item{\code{panelLayout}:}{layout (rows and columns) of
  panels of each plot.  Can be a length 2 numeric vector or
  a length 0 vector, in which case it is not used.}

  \item{\code{maxPanels}:}{Maximum number of panels to
  display on each page. Overrides layout if of length > 0.}
  }
}

\section{Methods}{
  \describe{ \item{show}{\code{signature(object =
  "multiTrellis")}: This print method plots the object} }
}
\examples{
\dontrun{
showClass("multiTrellis")
}
}
\author{
Mango Solutions
}

