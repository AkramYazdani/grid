
# A "gpar" object is a list of graphics parameters
# A graphics parameter is a name-value pair

gpar <- function(...) {
  gp <- validGP(list(...))
  class(gp) <- "gpar"
  gp
}

is.gpar <- function(x) {
  inherits(x, "gpar")
}

validGP <- function(gpars) {
  if (!is.na(match("font", names(gpars))))
    gpars$font <- as.integer(gpars$font)
  gpars
}

saved.pars <- function(pars) {
  list(prev=NULL, pars=pars)
}
push.saved.gpars <- function(gpars) {
  sp <- saved.pars(gpars)
  sp$prev <- .Call("L_getGPsaved")
  .Call("L_setGPsaved", sp)
}

pop.saved.gpars <- function() {
  .Call("L_setGPsaved", .Call("L_getGPsaved")$prev)
}

# possible gpar names
.grid.gpar.names <- c("fill", "col", "gamma", "lty", "lwd", "cex",
                      "fontsize", "lineheight", "font")

# Set .grid.gpars to keep grid record of current settings
set.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  subset <- match(names(gp), .grid.gpar.names)
  cur.gpars <- .Call("L_getGPar")
  push.saved.gpars(cur.gpars[subset])
  temp <- cur.gpars
  temp[subset] <- gp
  # Do this as a .Call.graphics to get it onto the base display list
  .Call.graphics("L_setGPar", temp)
}

unset.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  # for debugging really
  subset <- match(names(gp), .grid.gpar.names)
  saved.gpars <- .Call("L_getGPsaved")
  if (length(subset) != length(saved.gpars$pars))
    stop(paste("Trying to reset", names(gp),
               "with", saved.gpars$pars))
  temp <- .Call("L_getGPar")
  temp[subset] <- saved.gpars$pars
  # Do this as a .Call.graphics to get it onto the base display list
  .Call.graphics("L_setGPar", temp)
  pop.saved.gpars()
}  

get.gpar <- function(gpar.name) {
  .Call("L_getGPar")[[gpar.name]]
}


