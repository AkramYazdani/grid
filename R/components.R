######################################
# Default COLLECTION of grobs
######################################
draw.details.collection <- function(collection, grob, recording=TRUE) {
  # A collection draws all of its children
  lapply(collection$children, grid.draw, recording=FALSE)
}

# Have a draw=T argument because "only" other alternative is to
# have a separate make.collection function with identical argument
# list (i.e., duplicated entry point).  Not such an issue here,
# but just gets worse the more complex the graphical object gets.
grid.collection <- function(..., gp=gpar(), draw=T, vp=NULL) {
  children <- list(...)
  # Allow for single argument of a list of grobs (rather than
  # multiple grobs as separate arguments)
  if (!is.grob(children[[1]]) && is.list(children[[1]]))
    children <- children[[1]]
  collection <- list(children=children, gp=gp, vp=vp)
  cl <- "collection"
  grid.grob(collection, cl, draw)
}

######################################
# Simple "side-effect" plotting functions         
######################################

grid.grill <- function(h=unit(seq(0.25, 0.75, 0.25), "npc"),
                       v=unit(seq(0.25, 0.75, 0.25), "npc"),
                       default.units="npc",
                       gp=gpar(col="grey"), vp=NULL) {
  if (!is.unit(h))
    h <- unit(h, default.units)
  if (!is.unit(v))
    v <- unit(v, default.units)
  # FIXME:  Should replace for loop and call to grid.lines with call to grid.segments
  # once the latter exists
  push.viewport(vp)
  grid.segments(v, unit(0, "npc"), v, unit(1, "npc"), gp=gp)
  grid.segments(unit(0, "npc"), h, unit(1, "npc"), h, gp=gp)
  pop.viewport(vp)
}

######################################
# AXES
######################################

# NOTE that the `at' parameter is numeric (i.e., NOT a unit) for
# lxaxis and grid.yaxis.  These functions assume a unit for the `at'
# values rather than letting the user specify a unit.

common.draw.axis <- function(axis) {
  grid.draw(axis$major, recording=FALSE)
  grid.draw(axis$ticks, recording=FALSE)
  if (!is.null(axis$labels))
    grid.draw(axis$labels, recording=FALSE)
}

draw.details.xaxis <- function(axis, grob, recording=TRUE) {
  # We may have to create the children if there was not
  # enough information available at creation time
  if (is.na(axis$at)) {
    at <- .Call("L_pretty", current.viewport()$xscale)
    # We edit the grob itself so that the change is permanent
    grid.edit(grob, at=at, redraw=FALSE)
    # Then we make sure the current draw is aware of the change
    axis <- grid.get(grob)
  }    
  common.draw.axis(axis)
}

# NOTE that this can't be for all axes because it needs to
# call make.XAXIS.ticks and make.XAXIS.labels
edit.details.xaxis <- function(axis, new.values) {
  slot.names <- names(new.values)
  if (match("at", slot.names, nomatch=0)) {
    # NOTE that grid.edit has already set axis$at to the new value
    # We might set at to NULL to get ticks recalculated at redraw
    if (!is.na(axis$at)) {
      axis$major <- make.xaxis.major(axis$at, axis$main)
      axis$ticks <- make.xaxis.ticks(axis$at, axis$main)
      if (axis$label)
        axis$labels <- make.xaxis.labels(axis$at, axis$main)
      else
        axis$labels <- NULL
    }
  }
  # FIXME:  Handle "label=" and "main=" too ?
  axis
}

make.xaxis.major <- function(at, main) {
  if (main)
    y <- c(0, 0)
  else
    y <- c(1, 1)
  grid.lines(unit(c(min(at), max(at)), "native"),
         unit(y, "npc"), draw=FALSE)
}
    
make.xaxis.ticks <- function(at, main) {
  if (main) {
    tick.y0 <- unit(0, "npc")
    tick.y1 <- unit(-.5, "lines")
  }
  else {
    tick.y0 <- unit(1, "npc")
    tick.y1 <- unit(1, "npc") + unit(.5, "lines")
  }
  ticks <- grid.segments(unit(at, "native"), tick.y0,
                     unit(at, "native"), tick.y1,
                     draw=FALSE)
}

make.xaxis.labels <- function(at, main) {
  # FIXME:  labels only character versions of "at"
  if (main)
    label.y <- unit(-1.5, "lines")
  else
    label.y <- unit(1, "npc") + unit(1.5, "lines")
  grid.text(as.character(at), unit(at, "native"), label.y,
                    just="centre", rot=0, 
                    check.overlap=TRUE, draw=FALSE)
}

# The "main" x-axis is on the bottom when vp$origin is "bottom.*"
# and on the top when vp$origin is "top.*"
grid.xaxis <- function(at=NA, label = TRUE, main=TRUE, gp=gpar(),
                   draw=TRUE, vp=NULL) {
  if (is.na(at))
    if (is.null(vp)) {
      # We do not have enough information to make the ticks and labels
      major <- NULL
      ticks <- NULL
      labels <- NULL
    }
    else
      at <- .Call("L_pretty", vp$xscale)
  if (!is.na(at)) {
    major <- make.xaxis.major(at, main)
    ticks <- make.xaxis.ticks(at, main)
    if (label)
      labels <- make.xaxis.labels(at, main)
    else
      labels <- NULL
  }
  grid.grob(list(at=at, major=major, ticks=ticks, labels=labels,
             label=label, gp=gp, main=main, vp=vp),
        c("xaxis", "axis"), draw)
}

draw.details.yaxis <- function(axis, grob, recording=TRUE) {
  # We may have to create the children if there was not
  # enough information available at creation time
  if (is.na(axis$at)) {
    at <- .Call("L_pretty", current.viewport()$yscale)
    grid.edit(grob, at=at, redraw=FALSE)
    axis <- grid.get(grob)
  }    
  common.draw.axis(axis)
}

edit.details.yaxis <- function(axis, new.values) {
  slot.names <- names(new.values)
  if (match("at", slot.names, nomatch=0)) {
    if (!is.na(axis$at)) {
      axis$major <- make.yaxis.major(axis$at, axis$main)
      axis$ticks <- make.yaxis.ticks(axis$at, axis$main)
      if (axis$label)
        axis$labels <- make.yaxis.labels(axis$at, axis$main)
      else
        axis$labels <- NULL
    }
  }
  axis
}

make.yaxis.major <- function(at, main) {
  if (main)
    x <- c(0, 0)
  else
    x <- c(1, 1)
  grid.lines(unit(x, "npc"), unit(c(min(at), max(at)), "native"), draw=FALSE)
}
    
make.yaxis.ticks <- function(at, main) {
  if (main) {
    tick.x0 <- unit(0, "npc")
    tick.x1 <- unit(-.5, "lines")
  }
  else {
    tick.x0 <- unit(1, "npc")
    tick.x1 <- unit(1, "npc") + unit(.5, "lines")
  }
  ticks <- grid.segments(tick.x0, unit(at, "native"), 
                     tick.x1, unit(at, "native"),
                     draw=FALSE)
}

make.yaxis.labels <- function(at, main) {
  if (main) {
    hjust <- "right"
    label.x <- unit(-1, "lines")
  }
  else {
    hjust <- "left"
    label.x <- unit(1, "npc") + unit(1, "lines")
  }
  just <- c(hjust, "centre")
  grid.text(as.character(at), label.x, unit(at, "native"), 
        just=just, rot=0, check.overlap=TRUE, draw=FALSE)
}

# The "main" y-axis is on the left when vp$origin is "*.left"
# and on the right when vp$origin is "*.right"
grid.yaxis <- function(at=NA, label=TRUE, main=TRUE, gp=gpar(),
                   draw=TRUE, vp=NULL) {
  if (is.na(at))
    if (is.null(vp)) {
      # We do not have enough information to make the ticks and labels
      major <- NULL
      ticks <- NULL
      labels <- NULL
    }
    else
      at <- .Call("L_pretty", vp$yscale)
  if (!is.na(at)) {
    major <- make.yaxis.major(at, main)
    ticks <- make.yaxis.ticks(at, main)
    if (label)
      labels <- make.yaxis.labels(at, main)
    else
      labels <- NULL
  }
  grid.grob(list(at=at, major=major, ticks=ticks, labels=labels,
             label=label, gp=gp, main=main, vp=vp),
        c("yaxis", "axis"), draw)
}

