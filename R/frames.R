######################################
# Stuff for lpack()
######################################

grid.width <- function(x) {
  width(get.value(x))
}

grid.height <- function(x) {
  height(get.value(x))
}

width <- function(x) {
  UseMethod("width")
}

height <- function(x) {
  UseMethod("height")
}

width.default <- function(x) {
  unit(1, "null")
}

height.default <- function(x) {
  unit(1, "null")
}

width.frame <- function(frame) {
  sum(layout.widths(viewport.layout(frame$frame.vp)))
}

height.frame <- function(frame) {
  sum(layout.heights(viewport.layout(frame$frame.vp)))
}

draw.frame.child <- function(grob) {
  if (is.null(grob$border)) 
    temp.vp <- viewport(layout.pos.col=grob$col,
                         layout.pos.row=grob$row)
  else
    temp.vp <- viewport(layout.pos.col=grob$col,
                         layout.pos.row=grob$row,
                         x=grob$border[2],
                         y=grob$border[1],
                         width=unit(1, "npc") - sum(grob$border[c(2,4)]),
                         height=unit(1, "npc") - sum(grob$border[c(1,3)]))
  push.viewport(temp.vp, recording=FALSE)
  grid.draw(grob, recording=FALSE)
  pop.viewport(temp.vp, recording=FALSE)
}

draw.details.frame <- function(frame, grob, recording=TRUE) {
  push.viewport(frame$frame.vp, recording=FALSE)
  lapply(frame$children, draw.frame.child)
  pop.viewport(frame$frame.vp, recording=FALSE)
}

edit.details.frame <- function(frame, new.values) {
  # All we want to do here is make sure that children= and vp=
  # new.values are consumed
  slot.names <- names(new.values)
  if (c.index <- match("children", slot.names, nomatch=0)) {
    new.values <- new.values[-c.index]
    slot.names <- slot.names[-c.index]
  }
  if (fv.index <- match("frame.vp", slot.names, nomatch=0)) {
    new.values <- new.values[-fv.index]
    slot.names <- slot.names[-fv.index]
  }
  if (v.index <- match("vp", slot.names, nomatch=0))
    new.values <- new.values[-v.index]
  # Then we do what "collection"s do and pass everything down to children
  NextMethod()
}

# NOTE that this never produces any actual graphical output
# (there is nothing to draw) BUT it is important to use
# draw=TRUE if you want to pack the frame interactively.
# This ensures that the frame is on the .grid.display.list
# so that the editing that occurs in grid.pack() will redraw the
# frame when it forces a draw.all()
grid.frame <- function(vp=NULL, gp=gpar(), draw=TRUE) {
  # NOTE we have our own edit.details.frame, but in that
  # we want to be able to call edit.details.collection
  grid.grob(list(children=NULL, vp=vp, gp=gp, frame.vp=NULL),
        c("frame", "collection"), draw=draw)
}

num.col.specs <- function(side, col, col.before, col.after) {
  4 - sum(is.null(side) || any(c("top", "bottom") %in% side),
          is.null(col), is.null(col.before), is.null(col.after))
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
col.spec <- function(side, col, col.before, col.after, ncol) {
  if (!is.null(side)) {
    if (side == "left")
      col <- 1
    else if (side == "right")
      col <- ncol + 1
  }
  else if (!is.null(col.before))
    col <- col.before
  else if (!is.null(col.after))
    col <- col.after + 1
  col
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
new.col <- function(side, col, col.before, col.after, ncol) {
  # Special case ncol==0 for first grob added to frame
  if (!is.null(col))
    if (col > ncol)
      TRUE
    else
      FALSE
  else
    TRUE
}

num.row.specs <- function(side, row, row.before, row.after) {
  4 - sum(is.null(side) || any(c("left", "right") %in% side),
          is.null(row), is.null(row.before), is.null(row.after))
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
row.spec <- function(side, row, row.before, row.after, nrow) {
  if (!is.null(side)) {
    if (side == "top")
      row <- 1
    else if (side == "bottom")
      row <- nrow + 1
  }
  else if (!is.null(row.before))
    row <- row.before
  else if (!is.null(row.after))
    row <- row.after + 1
  row
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
new.row <- function(side, row, row.before, row.after, nrow) {
  # Special case nrow==0 for first grob added to frame
  if (!is.null(row))
    if (row > nrow)
      TRUE
    else
      FALSE
  else
    TRUE
}

mod.dims <- function(dim, dims, index, new.index, nindex) {
  if (new.index)
    if (index == 1)
      dims <- unit.c(dim, dims)
    else if (index == nindex)
      dims <- unit.c(dims, dim)
    else
      dims <- unit.c(dims[1:(index-1)], dim, dims[index:nindex])
  else {
    dim <- max(dim, dims[index])
    if (index==1)
      if (nindex == 1)
        dims <- dim
      else
        dims <- unit.c(dim, dims[2:nindex])
    else if (index==nindex)
      dims <- unit.c(dims[1:(nindex-1)], dim)
    else
      dims <- unit.c(dims[1:(index-1)], dim, dims[(index+1):nindex])
  }
  dims
}

# Pack a child grob within a frame grob
# (a special sort of editing just for frame grobs)
# FIXME:  Allow row/col specifications of length > 1
# FIXME:  Allow specification of respect for new row/col
grid.pack <- function(frame, grob, grob.name="", draw=TRUE,
                  side=NULL,
                  row=NULL, row.before=NULL, row.after=NULL,
                  col=NULL, col.before=NULL, col.after=NULL,
                  width=NULL, height=NULL,
                  border=NULL) {
  # (i) Check that the specifications of the location of the grob
  # give a unique location
  ncs <- num.col.specs(side, col, col.before, col.after)
  # If user does not specify a col, assume it is col 1 (for now ...)
  if (ncs == 0) {
    col <- 1
    ncs <- 1
  }
  if (ncs != 1) 
    stop("Cannot specify more than one of side=[\"left\", \"right\"], col, col.before, or col.after")
  nrs <- num.row.specs(side, row, row.before, row.after)
  # If user does not specify a row, assume it is row 1 (for now ...)
  if (nrs == 0) {
    row <- 1
    nrs <- 1
  }
  if (nrs != 1)
    stop("Must specify exactly one of side=[\"top\", \"bottom\"], row, row.before, or row.after")

  frame.vp <- grid.get(frame, "frame.vp")
  if (is.null(frame.vp))
    frame.vp <- viewport()
  lay <- viewport.layout(frame.vp)
  if (is.null(lay)) {
    ncol <- 0
    nrow <- 0
  } else {
    ncol <- layout.ncol(lay) 
    nrow <- layout.nrow(lay) 
  }
  
  # (ii) Determine that location and check that it is valid
  new.col <- new.col(side, col, col.before, col.after, ncol)
  col <- col.spec(side, col, col.before, col.after, ncol)
  if (col < 1 || col > ncol + 1)
    stop("Invalid column specification")
  new.row <- new.row(side, row, row.before, row.after, nrow)
  row <- row.spec(side, row, row.before, row.after, nrow)
  if (row < 1 || row > nrow + 1)
    stop("Invalid row specification")
  
  # (iii) If width and height are not given, take them from the child
  if (is.null(width))
    if (is.null(grob))
      width <- unit(1, "null")
    else
      width <- grid.width(grob)
  # FIXME:  What do you do with "width" if length(col) > 1 ???
  if (is.null(height))
    if (is.null(grob))
      height <- unit(1, "null")
    else
      height <- grid.height(grob)
  # FIXME:  What do you do with "height" if length(row) > 1 ???
  # If there is a border, include it in the width/height
  if (!is.null(border)) {
    width <- sum(border[2], width, border[4])
    height <- sum(border[1], height, border[3])
  }
  
  # (iv) Update the frame.vp of the frame (possibly add new row/col,
  # possibly update existing widths/heights and respect)
  if (new.col) ncol <- ncol + 1
  if (new.row) nrow <- nrow + 1
  if (is.null(lay)) {
    widths <- width
    heights <- height
  } else {
    widths <- mod.dims(width, layout.widths(lay), col, new.col, ncol)
    heights <- mod.dims(height, layout.heights(lay), row, new.row, nrow)
  }
  respect <- layout.respect(lay)
  viewport.layout(frame.vp) <- grid.layout(ncol=ncol, nrow=nrow,
                                       widths=widths, height=heights)
  children <- grid.get(frame, "children")
  if (!is.null(grob)) {
    grob$row <- row
    grob$col <- col
    grob$border <- border
    children <- c(children, list(grob))
  }
  grid.edit(frame, grid.prop.list(children=children, frame.vp=frame.vp), redraw=draw)
}

