\name{gpar}
\alias{gpar}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to produce a Graphical Parameter Object}
\description{
  This function returns an object of class \code{"gpar"}.  This is
  basically a list of name-value pairs.
}
\usage{
gpar(...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{\dots}{ Any number of named arguments. }
}
\details{
  All grid viewports and (predefined) graphical objects have a slot
  called \code{gp}, which contains a \code{"gpar"} object.  When
  a viewport is pushed onto the viewport stack and when a graphical object
  is drawn, the settings in the \code{"gpar"} object are enforced.
  In this way, the graphical output is modified by the \code{gp}
  settings until the graphical object has finished drawing, or until the
  viewport is popped off the viewport stack, or until some other
  viewport or graphical object is pushed or begins drawing.

  Valid parameter names are:
  \tabular{ll}{
    col \tab Colour for lines and borders. \cr
    fill \tab Colour for filling rectangles, polygons, ... \cr
    lty \tab Line type \cr
    lwd \tab Line width \cr
    fontsize \tab The size of text (in points) \cr
    lineheight \tab The height of a line as a multiple of fontsize \cr
    font \tab Font face \cr
  }
}
\value{
  An object of class \code{"gpar"}.
}
\author{Paul Murrell}
\examples{
gpar(col="red")
gpar(col="blue", lty="solid", lwd=3, fontsize=16)
grid.newpage()
vp <- viewport(w=.8, h=.8, gp=gpar(col="blue"))
grid.collection(grid.rect(gp=gpar(col="red"), draw=F),
                grid.text("The rect is its own colour (red)\nbut this text is the colour\nset by the collection (green)", draw=F),
                gp=gpar(col="green"), vp=vp)
grid.text("This text is the colour set by the viewport (blue)",
          y=1, just=c("center", "bottom"), gp=gpar(fontsize=20), vp=vp)
}
\keyword{ dplot }%-- one or more ...
