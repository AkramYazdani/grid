
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
  # Check a gpar is numeric and not NULL
  numnotnull <- function(gparname) {
    if (!is.na(match(gparname, names(gpars)))) {
      if (is.null(gpars[[gparname]]))
        gpars[[gparname]] <<- NULL
      else 
        gpars[[gparname]] <<- as.numeric(gpars[[gparname]])
    }
  }
  # fontsize, lineheight, cex, lwd should be numeric and not NULL
  numnotnull("fontsize")
  numnotnull("lineheight")
  numnotnull("cex")
  numnotnull("lwd")
  numnotnull("gamma")
  # col and fill are converted in C code
  # so is lty, BUT still want to check for NULL
  if (!is.na(match("lty", names(gpars)))) 
    if (is.null(gpars$lty))
      gpars$lty <- NULL  
  # font should be integer and not NULL
  if (!is.na(match("font", names(gpars)))) {
    if (is.null(gpars$font))
      gpars$font <- NULL
    else
      gpars$font <- as.integer(gpars$font)
  }
  gpars
}

saved.pars <- function(pars) {
  list(prev=NULL, pars=pars)
}
push.saved.gpars <- function(gpars) {
  sp <- saved.pars(gpars)
  sp$prev <- grid.Call("L_getGPsaved")
  grid.Call("L_setGPsaved", sp)
}

pop.saved.gpars <- function() {
  grid.Call("L_setGPsaved", grid.Call("L_getGPsaved")$prev)
}

# possible gpar names
.grid.gpar.names <- c("fill", "col", "gamma", "lty", "lwd", "cex",
                      "fontsize", "lineheight", "font")

# Set .grid.gpars to keep grid record of current settings
set.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  subset <- match(names(gp), .grid.gpar.names)
  cur.gpars <- grid.Call("L_getGPar")
  push.saved.gpars(cur.gpars[subset])
  temp <- cur.gpars
  temp[subset] <- gp
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics("L_setGPar", temp)
}

unset.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("Argument must be a 'gpar' object")
  # for debugging really
  subset <- match(names(gp), .grid.gpar.names)
  saved.gpars <- grid.Call("L_getGPsaved")
  if (length(subset) != length(saved.gpars$pars))
    stop(paste("Trying to reset", names(gp),
               "with", saved.gpars$pars))
  temp <- grid.Call("L_getGPar")
  temp[subset] <- saved.gpars$pars
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics("L_setGPar", temp)
  pop.saved.gpars()
}  

get.gpar <- function(gpar.name) {
  grid.Call("L_getGPar")[[gpar.name]]
}


