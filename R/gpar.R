
# A "gpar" object is a list of graphics parameters
# A graphics parameter is a name-value pair

gpar <- function(...) {
  gp <- list(...)
  class(gp) <- "gpar"
  gp
}

is.gpar <- function(x) {
  inherits(x, "gpar")
}

# A global record of the current settings of the grid graphics parameters
# NOTE that the order of these MUST match the order in .grid.gpar.names
.grid.gpars <- list(fill=NULL,    
                       col="black",      
                       lty="solid",     
                       lwd=1,
                       cex=1,
                       fontsize=10,          
                       lineheight=1.2)

# A record of gpar changes for reverting the changes
saved.pars <- function(pars) {
  list(prev=NULL, pars=pars, nxt=NULL)
}

.grid.saved.Rpars <- saved.pars(list())
.grid.saved.gpars <- saved.pars(gpar())
  
push.saved.Rpars <- function(Rpars) {
  sp <- saved.pars(Rpars)
  sp$prev <- .grid.saved.Rpars
  .grid.saved.Rpars$nxt <<- sp
  .grid.saved.Rpars <<- sp
}

pop.saved.Rpars <- function() {
  .grid.saved.Rpars <<- .grid.saved.Rpars$prev
}

push.saved.gpars <- function(gpars) {
  sp <- saved.pars(gpars)
  sp$prev <- .grid.saved.gpars
  .grid.saved.gpars$nxt <<- sp
  .grid.saved.gpars <<- sp
}

pop.saved.gpars <- function() {
  .grid.saved.gpars <<- .grid.saved.gpars$prev
}

# possible gpar names
# NOTE that names at the end of the list (e.g., "lineheight", have no par()
# counterpart and have no effect on par() settings
.grid.gpar.names <- c("fill", "col", "lty", "lwd", "cex",
                      "fontsize", "lineheight")
# par() names corresponding to the gpar names
.grid.Rpar.names <- c("bg",   "col", "lty", "lwd", "cex",
                      "ps",       "")
# convert a list of gpar$gpars into something compatible with par()
convert.gpar <- function(gp) {
  result <- gp
  names(result) <- .grid.Rpar.names[match(names(gp),
                                             .grid.gpar.names, nomatch="")]
  if (length(result) > 0)
    result <- result[!unlist(lapply(result, is.null))]
  result
}

# Set par() to actually have an effect on the output
# Set .grid.gpars to keep grid record of current settings
# (need this for lineheight to have effect because R has no equivalent)
set.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  push.saved.Rpars(par(convert.gpar(gp)))
  push.saved.gpars(.grid.gpars[match(names(gp), .grid.gpar.names)])
  .grid.gpars[match(names(gp), .grid.gpar.names)] <<- gp
}

unset.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  par(.grid.saved.Rpars$pars)
  pop.saved.Rpars()
  # for debugging really
  subset <- match(names(gp), .grid.gpar.names)
  if (length(subset) != length(.grid.saved.gpars$pars))
    stop(paste("Trying to reset", names(gp),
               "with", .grid.saved.gpars$pars))
  .grid.gpars[subset] <<- .grid.saved.gpars$pars
  pop.saved.gpars()
}  

get.gpar <- function(gpar.name) {
  .grid.gpars[[gpar.name]]
}


