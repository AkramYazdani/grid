is.viewport <- function(vp) {
  inherits(vp, "viewport")
}

valid.viewport <- function(x, y, width, height, just, origin,
                           gp,
                           xscale, yscale, angle,
                           layout, layout.pos.row, layout.pos.col) {
  if (unit.length(x) > 1 || unit.length(y) > 1 ||
      unit.length(width) > 1 || unit.length(height) > 1)
    stop("`x', `y', `width', and `height' must all be units of length 1")
  if (!is.gpar(gp))
    stop("Invalid graphics parameters")
  if (!is.numeric(xscale) || length(xscale) != 2)
    stop("Invalid xscale in viewport")
  if (!is.numeric(yscale) || length(yscale) != 2)
    stop("Invalid yscale in viewport")
  if (!is.numeric(angle) || length(angle) != 1)
    stop("Invalid angle in viewport")
  if (!is.null(layout.pos.row))
    layout.pos.row <- as.integer(rep(layout.pos.row, length.out=2))
  if (!is.null(layout.pos.col))
    layout.pos.col <- as.integer(rep(layout.pos.col, length.out=2))
  # Put all the valid things first so that are found quicker
  vp <- list(x = x, y = y, width = width, height = height,
             valid.just = valid.just(just, 2),
             valid.origin = valid.origin(origin),
             layout = layout,
             valid.pos.row = layout.pos.row,
             valid.pos.col = layout.pos.col,
             gp = gp,
             # A viewport may have a specification of fontsize
             # and lineheight in the gpar, BUT it does not have to
             # If it does not, then that means it will just use
             # whatever is the "current" setting of fontsize
             # and lineheight.
             # "current" means at drawing time, which means when
             # set.viewport is called.
             # We record here the "current" value so that we can
             # reset the value when a child viewport is popped.
             cur.fontsize = NULL,
             cur.lineheight = NULL,
             xscale = xscale,
             yscale = yscale,
             angle = angle,
             parent = NULL,
             justification = just,
             origin = origin,
             layout.pos.row = layout.pos.row,
             layout.pos.col = layout.pos.col)
  class(vp) <- "viewport"
  vp
}

print.viewport <- function(vp) {
  print(class(vp))
}

####################
# Accessors
####################

viewport.layout <- function(vp) {
  vp$layout
}

"viewport.layout<-" <- function(vp, value) {
  # FIXME:  should check that new layout value is valid
  vp$layout <- value
  vp
}

####################
# Public Constructor
####################
viewport <- function(x = unit(0.5, "npc"),
                     y = unit(0.5, "npc"),
                     width = unit(1, "npc"),
                     height = unit(1, "npc"),
                     default.units = "npc",
                     just = "centre",
                     origin = "bottom.left",
                     gp = gpar(),
                     # FIXME: scales are only linear at the moment 
                     xscale = c(0, 1),
                     yscale = c(0, 1),
                     angle = 0,
                     # Layout for arranging children of this viewport
                     layout = NULL,
                     # Position of this viewport in parent's layout
                     layout.pos.row = NULL,
                     layout.pos.col = NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  valid.viewport(x, y, width, height, just, origin,
                 gp, xscale, yscale, angle,
                 layout, layout.pos.row, layout.pos.col)
}

