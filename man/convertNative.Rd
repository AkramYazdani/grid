\name{convertNative}
\alias{convertNative}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Convert a Unit Object to Native units }
\description{
  This function returns a numeric vector containing the specified
  x or y locations or dimensions, converted to "user" or "data"
  units, relative to the current viewport.
}
\usage{
convertNative(unit, dimension="x", type="location")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{unit}{ A unit object. }
  \item{dimension}{ Either "x" or "y". }
  \item{type}{ Either "location" or "dimension". }
}
\value{
  A numeric vector.
}
\author{ Paul Murrell }
\section{WARNING }{ If you draw objects based on output from these conversion
    functions, then resize your device, the objects will be drawn 
    incorrectly -- the base R display list will not recalculate these
    conversions.  This means that you can only rely on the results of 
    these calculations if the size of your device is fixed.} 
\seealso{ \code{\link{unit}} }

\examples{
push.viewport(viewport(width=unit(.5, "npc"),
                       height=unit(.5, "npc")))
grid.rect()
w <- convertNative(unit(1, "inches"))
h <- convertNative(unit(1, "inches"), "y")
# This rectangle starts off life as 1in square, but if you
# resize the device it will no longer be 1in square
grid.rect(width=unit(w, "native"), height=unit(h, "native"),
          gp=gpar(col="red"))
pop.viewport(1)
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
