######################################
# move-to and line-to primitives
######################################
draw.details.move.to <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_moveTo", x$x, x$y)
}

grid.move.to <- function(x=0, y=0,
                         default.units="npc",
                         draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  # Make sure that x and y are of length 1
  if (unit.length(x) > 1 | unit.length(y) > 1)
    stop("x and y must have length 1")
  grid.grob(list(x=x, y=y, vp=vp), "move.to", draw)
}

draw.details.line.to <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_lineTo", x$x, x$y)
}

grid.line.to <- function(x=1, y=1,
                         default.units="npc",
                         draw=TRUE, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  # Make sure that x and y are of length 1
  if (unit.length(x) > 1 | unit.length(y) > 1)
    stop("x and y must have length 1")
  grid.grob(list(x=x, y=y, gp=gp, vp=vp), "line.to", draw)
}

######################################
# LINES primitive
######################################
draw.details.lines <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_lines", x$x, x$y)
}

# Specify "units.per.obs=TRUE" to give a unit or units per (x, y) pair
grid.lines <- function(x=unit(c(0, 1), "npc", units.per.obs),
                   y=unit(c(0, 1), "npc", units.per.obs),
                   default.units="npc", units.per.obs=FALSE,
                   gp=gpar(), draw=TRUE, vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x))
    x <- unit(x, default.units, units.per.obs)
  if (!is.unit(y))
    y <- unit(y, default.units, units.per.obs)
  l <- list(x=x, y=y, gp=gp, vp=vp)
  cl <- "lines"
  grid.grob(l, cl, draw)
}

######################################
# SEGMENTS primitive
######################################
draw.details.segments <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_segments", x$x0, x$y0, x$x1, x$y1)
}

# Specify "units.per.obs=TRUE" to give a unit or units per (x, y) pair
grid.segments <- function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                      x1=unit(1, "npc"), y1=unit(1, "npc"),
                      default.units="npc", units.per.obs=FALSE,
                      gp=gpar(), draw=TRUE, vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x0))
    x0 <- unit(x0, default.units, units.per.obs)
  if (!is.unit(x1))
    x1 <- unit(x1, default.units, units.per.obs)
  if (!is.unit(y0))
    y0 <- unit(y0, default.units, units.per.obs)
  if (!is.unit(y1))
    y1 <- unit(y1, default.units, units.per.obs)
  s <- list(x0=x0, y0=y0, x1=x1, y1=y1, gp=gp, vp=vp)
  cl <- "segments"
  grid.grob(s, cl, draw)
}

######################################
# POLYGON primitive
######################################

draw.details.polygon <- function(x, x.wrapped, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  grid.Call.graphics("L_polygon", x$x, x$y)
}

grid.polygon <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                         default.units="npc", 
                         gp=gpar(),draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  p <- list(x=x, y=y, gp=gp, vp=vp)
  cl <- "polygon"
  grid.grob(p, cl, draw)
}

######################################
# CIRCLE primitive
######################################

draw.details.circle <- function(x, x.wrapped, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  grid.Call.graphics("L_circle", x$x, x$y, x$r)
}

grid.circle <- function(x=0.5, y=0.5, r=0.5,
                         default.units="npc", 
                         gp=gpar(),draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(r))
    r <- unit(r, default.units)
  c <- list(x=x, y=y, r=r, gp=gp, vp=vp)
  cl <- "circle"
  grid.grob(c, cl, draw)
}

######################################
# RECT primitive
######################################
draw.details.rect <- function(x, x.wrapped, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  grid.Call.graphics("L_rect", x$x, x$y, x$width, x$height,
                 valid.just(x$just))
}

width.details.rect <- function(x) {
  absolute.size(x$width)
}

height.details.rect <- function(x) {
  absolute.size(x$height)
}

grid.rect <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      width=unit(1, "npc"), height=unit(1, "npc"),
                      just="centre", default.units="npc", 
                      gp=gpar(),draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  r <- list(x=x, y=y, width=width, height=height, just=just, gp=gp, vp=vp)
  cl <- "rect"
  grid.grob(r, cl, draw)
}

######################################
# TEXT primitive
######################################
draw.details.text <- function(x, x.wrapped, recording=TRUE) {
  # FIXME:  Need type checking for "rot" and "check.overlap"
  grid.Call.graphics("L_text", x$label, x$x, x$y, 
                 valid.just(x$just), x$rot, x$check.overlap)
}

width.details.text <- function(x) {
  unit(1, "mystrwidth", data=x$label)
}

height.details.text <- function(x) {
  unit(1, "mystrheight", data=x$label)
}

grid.text <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                  just="centre", rot=0, check.overlap=FALSE,
                  default.units="npc", gp=gpar(), draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.expression(label))
    label <- as.character(label)
  txt <- list(label=label, x=x, y=y, gp=gp,
              just=just, rot=rot, check.overlap=check.overlap,
              vp=vp)
  cl <- "text"
  grid.grob(txt, cl, draw)
}

######################################
# POINTS primitive
######################################
draw.details.points <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_points", x$x, x$y, x$pch, x$size)
}

valid.pch <- function(pch) {
  if (is.null(pch))
    pch <- as.integer(1)
  else if (!is.character(pch))
    pch <- as.integer(pch)
  pch
}

grid.points <- function(x=runif(10),
                        y=runif(10),
                        pch=1, size=unit(1, "char"), 
                        default.units="native", gp=gpar(),
                        draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (unit.length(x) != unit.length(y))
    stop("x and y must be unit objects and have the same length")
  p <- list(x=x, y=y, pch=valid.pch(pch), size=size, gp=gp, vp=vp)
  cl <- "points"
  grid.grob(p, cl, draw)
}

