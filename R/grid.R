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
# Set newpage=FALSE if you want to do grid graphics without advancing a page
grid.start <- function(newpage=TRUE) {
  # NOTE that this starts a device as a side-effect
  .grid.saved.pars <<- par(xpd=NA, mfrow=c(1, 1),
                              oma=rep(0, 4), mar=rep(0, 4))
  # Install some default par settings
  set.gpar(gpar(fontsize=10, lineheight=1.2))
  if (newpage)
    grid.newpage()
  else
    .Call("L_initDevice")
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

grid.init.viewport.stack <- function(recording=TRUE) {
  # Create a viewport WITH default cur.lineheight and cur.fontsize
  # This is the top-level viewport
  # This function and push/pop.viewport should be the ONLY places
  # where set.viewport gets called
  # This is the ONLY viewport that gets sent to set.viewport
  # WITHOUT going via push/pop.viewport
  # Hence we must set its cur.lineheight/fontsize
  vp <- viewport()
  if (recording)
    record(vp)
  vp$cur.lineheight <- 1.2
  vp$cur.fontsize <- 10
  # NOTE that L_setviewport does 
  #   .grid.viewport <<- vp
  .Call.graphics("L_setviewport", vp, FALSE)
}

push.vp <- function(vps, index, len, recording) {
  vp <- vps[[index]]
  if (is.null(vp))
    stop("Illegal to push NULL viewport")
  # Record on the display list
  if (recording)
    record(vp)
  # Enforce gpar settings
  set.gpar(vp$gp)
  # Later, we will query the viewport to ask "what were the gpar
  # settings when you were drawn".  This is NOT the same as asking
  # the viewport for its gpar settings because the viewport may only
  # specify some gpar values.  So we record the default settings
  # we will need to know about
  vp$cur.fontsize <- par("ps")
  vp$cur.lineheight <- get.gpar("lineheight")
  # Calculate viewport transform 
  # NOTE that we will have modified "vp" within L_setviewport
  # to record the current transformation and layout
  # NOTE also that L_setviewport does 
  #   vp$parent <- .grid.viewport
  # ... calc transform ...
  #   .grid.viewport <<- vp
  .Call.graphics("L_setviewport", vp, TRUE)
  # Push further viewports if required
  if (index < len) 
    push.vp(vps, index+1, len, recording)
}

push.viewport <- function(..., recording=TRUE) {
  if (missing(...))
    stop("Must specify at least one viewport")
  else {
    vps <- list(...)
    nvp <- length(vps)
    push.vp(vps, 1, nvp, recording)
  }
}

pop.vp <- function(last.one, recording) {
  vp <- .grid.viewport
  # Fail if trying to pop top-level viewport
  if (is.null(vp$parent))
    stop("Illegal to pop top-level viewport")
  # Unset gpar settings
  unset.gpar(vp$gp)
  # Allow for recalculation of viewport transform if necessary
  # NOTE that L_checkviewport does
  #   new.vp <- .grid.viewport$parent
  # ... recalc viewport transform if necessary ...
  #   .grid.viewport <<- new.vp
  .Call.graphics("L_unsetviewport", last.one)
}

pop.viewport <- function(n=1, recording=TRUE) {
  if (n < 1)
    stop("Must pop at least one viewport")
  else {
    for (i in 1:n)
      pop.vp(i==n, recording)
    # Record on the display list
    if (recording)
      record(n)
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

# When there is a pop.viewport, the number of viewports popped
# gets put on the display list
record.default <- function(n) {
  inc.display.list()
  .grid.display.list[[.grid.display.list.index]] <<- n
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

