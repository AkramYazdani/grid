\name{grid.line.to}
\alias{grid.line.to}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Draw a Line to a Specified Position }
\description{
  Grid has the notion of a current location.  This function
  draws a line from the current location to the location specified
  in the function call \emph{and} sets the current location
  to be the location specified in the function call.}
}
\usage{
grid.line.to(x=1, y=1, default.units="npc", draw=TRUE, gp=gpar(), vp=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{default.units}{ ~~Describe \code{default.units} here~~ }
  \item{draw}{ ~~Describe \code{draw} here~~ }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{ ~~Describe \code{vp} here~~ }
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport},
  \code{\link{grid.move.to}}}
}

\examples{
}
\keyword{ dplot }%-- one or more ...
