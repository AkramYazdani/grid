
.First.lib <- function(lib, pkg) {
  library.dynam( "grid", pkg, lib )
  grid.start()
}

.Last.lib <-function(libpath) {
  grid.stop()
}
