######################################
# move-to and line-to primitives
######################################
draw.details.move.to <- function(l, grob, recording=TRUE) {
  grid.Call.graphics("L_moveTo", l$x, l$y)
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

draw.details.line.to <- function(l, grob, recording=TRUE) {
  grid.Call.graphics("L_lineTo", l$x, l$y)
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
draw.details.lines <- function(l, grob, recording=TRUE) {
  grid.Call.graphics("L_lines", l$x, l$y)
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
draw.details.segments <- function(s, grob, recording=TRUE) {
  grid.Call.graphics("L_segments", s$x0, s$y0, s$x1, s$y1)
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

draw.details.polygon <- function(p, grob, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  grid.Call.graphics("L_polygon", p$x, p$y)
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

draw.details.circle <- function(c, grob, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  grid.Call.graphics("L_circle", c$x, c$y, c$r)
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
draw.details.rect <- function(r, grob, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  grid.Call.graphics("L_rect", r$x, r$y, r$width, r$height,
                 valid.just(r$just, 2))
}

width.details.rect <- function(r) {
  absolute.size(r$width)
}

height.details.rect <- function(r) {
  absolute.size(r$height)
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
draw.details.text <- function(txt, grob, recording=TRUE) {
  # FIXME:  Need type checking for "rot" and "check.overlap"
  grid.Call.graphics("L_text", txt$label, txt$x, txt$y, 
                 valid.just(txt$just, 2), txt$rot, txt$check.overlap)
}

width.details.text <- function(txt) {
  unit(1, "mystrwidth", data=txt$label)
}

height.details.text <- function(txt) {
  unit(1, "mystrheight", data=txt$label)
}

grid.text <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                  just="centre", rot=0, check.overlap=FALSE,
                  default.units="npc", gp=gpar(), draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  txt <- list(label=as.character(label), x=x, y=y, gp=gp,
              just=just, rot=rot, check.overlap=check.overlap,
              vp=vp)
  cl <- "text"
  grid.grob(txt, cl, draw)
}

######################################
# POINTS primitive
######################################
draw.details.points <- function(p, grob, recording=TRUE) {
  grid.Call.graphics("L_points", p$x, p$y, p$pch, p$size)
}

valid.pch <- function(pch) {
  if (!is.character(pch))
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
  if (length(x) != length(y))
    stop("x and y must be unit objects and have the same length")
  p <- list(x=x, y=y, pch=valid.pch(pch), size=size, gp=gp, vp=vp)
  cl <- "points"
  grid.grob(p, cl, draw)
}

