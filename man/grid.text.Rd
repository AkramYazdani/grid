\name{grid.text}
\alias{grid.text}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Draw Text in a Grid Viewport}
\description{
  This function draws a piece of text.
}
\usage{
grid.text(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
          just = "centre", rot = 0,
          check.overlap = FALSE, default.units = "npc",
          gp=gpar(), draw = TRUE, vp = NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{label}{A vector of strings to draw.}
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{just}{The justification of the text about its (x, y)
    location.  If two values are given, the first specifies
    horizontal justification and the second specifies vertical
    justification.}
  \item{rot}{The angle to rotate the text.}
  \item{check.overlap}{A logical value to indicate whether to
    check for and omit overlapping text.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y}
    are only given as numeric vectors.}  
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  An object of class \code{"grob"}.
}
\details{
  The \code{"grob"} object contains an object of class \code{"text"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}

\examples{
}
\keyword{ dplot }%-- one or more ...
