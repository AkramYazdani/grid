
.First.lib <- function(lib, pkg) {
  library.dynam( "grid", pkg, lib )
  .Call("L_initGrid")
  .grid.loaded <<- TRUE
}

.Last.lib <-function(libpath) {
  if (.grid.loaded) {
    .Call("L_killGrid")
    # Kill all existing devices to avoid replay
    # of display list which tries to run grid code
    # Not very friendly to other registered graphics systems
    # but its safety first for now
    graphics.off()
  }
}

