\name{Grid}
\alias{Grid}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Grid Graphics}
\description{
  General information about the grid graphics package.
}
\usage{
}
\arguments{
}
%- maybe also `usage' for other objects documented here.
\details{
  Grid graphics provides an alternative to the standard R graphics.
  The user is able to define arbitrary rectangular regions (called
  \emph{viewports}) on the
  graphics device and define a number of coordinate systems for each
  region.  Drawing can be specified to occur in any viewport using
  any of the available coordinate systems.
  
  Grid graphics and standard R graphics do not mix!

  Type \code{library(help = grid)} to see a list of (public)
  Grid graphics functions.
}
\author{Paul Murrell}
\seealso{
  \code{\link{viewport}},
  \code{\link{grid.layout}}, and
  \code{\link{unit}}.
}
\examples{
# Diagram of a simple layout 
grid.show.layout(grid.layout(4,2,
                     heights=unit(rep(1, 4),
                                  c("lines", "lines", "lines", "null")),
                     widths=unit(c(1, 1), "inches")))
# Diagram of a sample viewport
grid.show.viewport(viewport(x=0.6, y=0.6,
                            w=unit(1, "inches"), h=unit(1, "inches")))
# A flash plotting example
grid.multipanel(vp=viewport(0.5, 0.5, 0.8, 0.8))
}

\keyword{dplot}%-- one or more ...

