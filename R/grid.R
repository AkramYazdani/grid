# FIXME:  All of these functions are calling par(xpd=NA, ...)
# Only the "bottom level" calls need to;  can I easily identify which
# are the "bottom level" ones ?

# NOTE that the valid.* functions are ONLY called just before
# parameters are passed in a .Call.graphics
# If you call a valid.* function on an already-validated unit or
# whatever, it will fail.

# FIXME:  all grid functions should check that .grid.started is TRUE
.grid.started <- FALSE
.grid.saved.pars <- NULL
# We rely on this being set to a valid viewport by a call to set.viewport()
# (in a call to lstart() in .First.lib)
.grid.viewport <- NULL

# Call this function before you do any grid graphics
# This function is called by .First.lib so in simple usage
# the user need never know about it.
# Simple usage means:
#     library(grid), <grid drawing>[, detach(package:grid)]
grid.start <- function() {
  # NOTE that this starts a device as a side-effect
  .grid.saved.pars <<- par(xpd=NA, mfrow=c(1, 1),
                              oma=rep(0, 4), mar=rep(0, 4))
  # Install some default par settings
  set.gpar(gpar(fontsize=10, lineheight=1.2))
  grid.newpage()
  .grid.started <<- TRUE
}

# Call this function once you have finished with grid graphics
# This function is called by .Last.lib so in simple usage
# the user need never know about it.
# Simple usage means:
#     library(grid), <grid drawing>, detach(package:grid)
grid.stop <- function() {
  par(.grid.saved.pars)
  .grid.started <<- FALSE
}

# Set the grid "current viewport"
# This viewport may have multiple parents stacked on top of it
# NOTE that this operation gets recorded in the ldisplay.list
set.viewport <- function(vp, recording=TRUE) {
  # valid.viewport will let a NULL viewport through and we do
  # NOT want that to happen here
  if (is.null(vp))
    stop("Illegal current viewport setting")
  .Call.graphics("L_setviewport", vp)
  .grid.viewport <<- vp
  if (recording)
    record(vp)
}

grid.init.viewport.stack <- function(recording=TRUE) {
  # Create a viewport WITH default cur.lineheight and cur.fontsize
  # This is the top-level viewport
  # This function and push/pop.viewport should be the ONLY places
  # where set.viewport gets called
  # This is the ONLY viewport that gets sent to set.viewport
  # WITHOUT going via push/pop.viewport
  # Hence we must set its cur.lineheight/fontsize
  vp <- viewport()
  vp$cur.lineheight <- 1.2
  vp$cur.fontsize <- 10
  set.viewport(vp, recording=recording)
}

stack.viewports <- function(vps) {
  if (length(vps) == 0)
    vp <- NULL
  else {
    vp <- vps[[length(vps)]]
    if (is.null(vp)) {
      if (length(vps) > 1)
        vp <- stack.viewports(vps[1:(length(vps)-1)])
    }
    else {
      if (length(vps) > 1)
        vp$parent <- stack.viewports(vps[1:(length(vps)-1)])
    }
  }
  # Enforce viewport's gpar settings
  # DON'T reinforce the gpar settings for the previously current viewport
  # (length(vps) == 1)
  if (!is.null(vp) & length(vps) != 1) {
    set.gpar(vp$gp)
    # Later, we will query the viewport to ask "what were the gpar
    # settings when you were drawn".  This is NOT the same as asking
    # the viewport for its gpar settings because the viewport may only
    # specify some gpar values.  So we record the default settings
    # we will need to know about
    vp$cur.fontsize <- par("ps")
    vp$cur.lineheight <- get.gpar("lineheight")
  }
  vp
}

# Push a viewport onto the viewport stack
      # NOTE that we must do setting of gpars here so that
      # the gpars of viewports and grobs are correctly
      # intertwined
push.viewport <- function(..., recording=TRUE) {
  # FIXME:  should probably be doing this internally because
  # the user can get at .grid.viewport
  # Check special case where ... is single NULL viewport
  if (missing(...))
    stop("Must specify at least one viewport")
  else {
    vps <- list(...)
    if (length(vps) != 1 || !is.null(vps[[1]])) {
      vps <- list(.grid.viewport, ...)
      vp <- stack.viewports(vps)
      set.viewport(vp, recording)
    }
  }
}

unstack.viewports <- function(startvp, vps) {
  if (!is.null(vps[[1]])) {
    unset.gpar(vps[[1]]$gp)
    startvp <- startvp$parent
  }
  if (length(vps) > 1)
    vp <- unstack.viewports(startvp, vps[2:length(vps)])
  else
    vp <- startvp
  vp
}

# Pop a viewport off the viewport stack
      # NOTE that we must do unsetting of gpars here so that
      # the gpars of viewports and grobs are correctly
      # intertwined
pop.viewport <- function(..., recording=TRUE) {
  # FIXME:  should probably be doing this internally because
  # the user can get at .grid.viewport
  # Check special case where ... is single NULL viewport
  if (missing(...))
    stop("Must specify at least one viewport")
  else {
    vps <- list(...)
    if (length(vps) != 1 || !is.null(vps[[1]])) {
      vp <- unstack.viewports(.grid.viewport, vps)
      set.viewport(vp, recording)
    }
  }
}

# Function to obtain the current viewport
# Grid plotting functions all take a viewport argument which
# currents to NULL (NULL indicates that the current viewport
# should be used).  The function may want to copy the viewport
# it is drawing into (see e.g., lxaxis and grid.yaxis) and this
# function provides a consistent interface for deciding whether
# a temporary viewport has been specified or whether the
# current viewport is being used.
# Can also be called without specifying vp, just to get current
# current viewport (see e.g., lgrid)
current.viewport <- function(vp=NULL) {
  if (is.null(vp))
    .grid.viewport
  else
    vp
}

clearpage <- function() {
  .Call("L_newpagerecording", par("ask"))
  .Call.graphics("L_newpage")
}

# Call this function if you want the graphics device erased or moved
# on to a new page.  High-level plotting functions should call this.
# NOTE however, that if you write a function which calls grid.newpage,
# you should provide an argument to allow people to turn it off
# so that they can use your function within a parent viewport
# (rather than the whole device) if they want to.
grid.newpage <- function(recording=TRUE) {
  clearpage()
  if (recording)
    # Erase the Grid display list
    clear.display.list()  
  # Reset the current viewport to be the entire device
  # (Is this an ok thing to do ?)
  grid.init.viewport.stack(recording=recording)
}

# Keep a list of all drawing operations (since last grid.newpage()) so
# that we can redraw upon edit.
# FIXME:  Need list like this PER DEVICE
.grid.display.list <- vector("list", 100)
.grid.display.list.index <- 0
# Flag to indicate whether to record graphics operations
# record() and clear.display.list() check this value before
# they do anything
.grid.display.list.on <- 1

inc.display.list <- function() {
  .grid.display.list.index <<- .grid.display.list.index + 1
  n <- length(.grid.display.list)
  if (.grid.display.list.index > n) {
    temp <- .grid.display.list
    .grid.display.list <<- vector("list", n+100)
    .grid.display.list[1:n] <<- temp
  }
}

# This will either ...
#   (i) turn on AND INITIALISE the display list or ...
#   (ii) turn off AND ERASE the display list
grid.display.list <- function(on=TRUE) {
  .grid.display.list.on <<- on
  if (on) {
    .grid.display.list <- vector("list", 100)
    .grid.display.list.index <- 0
  }
  else 
    .grid.display.list <<- NULL
  .grid.display.list
}

record <- function(x) {
  if (.grid.display.list.on)
    UseMethod("record")
}

record.grob <- function(grob) {
  inc.display.list()
  # FIXME:  Should use assign() here ?
  .grid.display.list[[.grid.display.list.index]] <<- grob
}

record.viewport <- function(vp) {
  inc.display.list()
  # FIXME:  Should use assign() here ?
  .grid.display.list[[.grid.display.list.index]] <<- vp
}

clear.display.list <- function() {
  if (.grid.display.list.on) {
    .grid.display.list <<- vector("list", 100)
    .grid.display.list.index <<- 0
  }
}

