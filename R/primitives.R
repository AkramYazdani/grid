######################################
# LINES primitive
######################################
draw.details.lines <- function(l, grob, recording=TRUE) {
  .Call.graphics("L_lines", l$x, l$y, .grid.viewport)
}

width.lines <- function(l) {
  max(l$x) - min(l$x)
}

height.lines <- function(l) {
  max(l$y) - min(l$y)
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
  .Call.graphics("L_segments", s$x0, s$y0, s$x1, s$y1,
                 .grid.viewport)
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
# RECT primitive
######################################
draw.details.rect <- function(r, grob, recording=TRUE) {
  # FIXME:  Here I am passing in the colours, whereas in lgrid below
  # I set the colours using par and never pass them down.  This is
  # inconsistent !  BUT due to inconsistency in graphics.c so this
  # is a FIXGRAPHICS rather than a FIXME :)
  .Call.graphics("L_rect", r$x, r$y, r$width, r$height,
                 valid.just(r$just, 2),
                 get.gpar("col"), get.gpar("fill"),
                 .grid.viewport)
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
  .Call.graphics("L_text", txt$label, txt$x, txt$y, 
                 valid.just(txt$just, 2), txt$rot, txt$check.overlap,
                 .grid.viewport)
}

width.text <- function(txt) {
  unit(1, "strwidth", data=txt$label)
}

height.text <- function(txt) {
  unit(1, "strheight", data=txt$label)
}

grid.text <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                  just="centre", rot=0, check.overlap=FALSE,
                  default.units="npc", gp=gpar(), draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  txt <- list(label=label, x=x, y=y, gp=gp,
              just=just, rot=rot, check.overlap=check.overlap,
              vp=vp)
  cl <- "text"
  grid.grob(txt, cl, draw)
}

######################################
# POINTS primitive
######################################
draw.details.points <- function(p, grob, recording=TRUE) {
  .Call.graphics("L_points", p$x, p$y, p$pch, p$size,
                 get.gpar("col"), get.gpar("fill"), 
                 .grid.viewport)
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
  p <- list(x=x, y=y, pch=pch, size=size, gp=gp, vp=vp)
  cl <- "points"
  grid.grob(p, cl, draw)
}

