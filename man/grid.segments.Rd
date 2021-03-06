\name{grid.segments}
\alias{grid.segments}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Draw Line Segments in a Grid Viewport }
\description{
  This function draws line segments.
}
\usage{
grid.segments(x0=0, y0=0, x1=1, y1=1,
              default.units="npc", units.per.obs=FALSE,
              gp=gpar(), draw=TRUE, vp=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x0}{ Numeric indicating the starting x-values of the line segments. }
  \item{y0}{ Numeric indicating the starting y-values of the line
    segments. }
  \item{x1}{ Numeric indicating the stopping x-values of the line
    segments. }
  \item{y1}{ Numeric indicating the stopping y-values of the line segments.}
  \item{gp}{ An object of class \code{gpar}. }
  \item{default.units}{ A string. }
  \item{units.per.obs}{ A boolean indicating whether distinct units are
    given for each x/y-value. }
  \item{draw}{ A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL)}
}
\value{
  An object of class \code{"grob"}.
}
\author{ Paul Murrell                }
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}

\examples{
}
\keyword{ dplot }%-- one or more ...
