
recycle.data <- function(data, data.per, max.n) {
  # VERY IMPORTANT:  Even if there is only one data specified
  # and/or only one data needed, we want this to be a LIST of
  # data values so that a single data and several data can be
  # handled equivalently
  # The test for whether it is only a single value currently
  # consists of a check for mode="character" (i.e., a single
  # string) or class="viewport" (i.e., a single viewport)
  if (is.character(data) || is.viewport(data)) 
    data <- list(data)
  if (data.per)
    n <- max.n
  else
    n <- length(data)
  original <- data
  index <- 1
  while (length(data) < n) {
    data <- c(data, list(original[[(index - 1) %% length(original) + 1]]))
    index <- index + 1
  }
  data
}

# Create an object of class "unit"
# Simple units are of the form `unit(1, "cm")' or `unit(1:3, "cm")' or
# `unit(c(1,3,6), c("cm", "inch", "npc"))'
# More complicated units are of the form `unit(1, "string", "a string")'
# or `unit(1, "viewport", vp)'
unit <- function(x, units, data=NULL) {
  if (!is.numeric(x))
    stop("x must be numeric")
  valid.unit(x, units, recycle.data(data, F, length(x)))
}

valid.unit <- function(x, units, data) {
  valid.units <- valid.units(units)
  data <- valid.data(units, data)
  attr(x, "unit") <- units
  attr(x, "valid.unit") <- valid.units
  attr(x, "data") <- data
  class(x) <- "unit"
  x
}

# NOTE: the order of the strings in these conversion functions must
# match the order of the enums in ../src/grid.h
.grid.unit.list <- c("npc", "cm", "inches", "lines",
                     "native", "null", "snpc", "mm",
                     "points", "picas", "bigpts",
                     "dida", "cicero", "scaledpts",
                     "strwidth", "strheight",
                     "vplayoutwidth", "vplayoutheight", "char")

# Make sure that and "str*" and "vplayout*" units have data
valid.data <- function(units, data) {
  n <- length(units)
  str.units <- units == "strwidth"
  if (any(str.units != 0))
    for (i in (1:n)[str.units])
      if (!(length(data) >= i && is.character(data[[i]])))
        stop("No string supplied for `strwidth' unit")
  str.units <- units == "strheight"
  if (any(str.units != 0))
    for (i in (1:n)[str.units])
      if (!(length(data) >= i && is.character(data[[i]])))
        stop("No string supplied for `strheight' unit")
  # Make sure that a viewport has been specified and that the
  # viewport has a layout
  layout.units <- units == "vplayoutwidth"
  if (any(layout.units != 0))
    for (i in (1:n)[layout.units]) {
      if (!(length(data) >= i && is.viewport(data[[i]])))
        stop("No viewport supplied for `vplayoutwidth' unit")
      else if (!is.layout(data[[i]]$layout))
        stop("No layout specified in viewport for `vplayoutwidth' unit")
    }
  layout.units <- units == "vplayoutheight"
  if (any(layout.units != 0))
    for (i in (1:n)[layout.units]) {
      if (!(length(data) >= i && is.viewport(data[[i]])))
        stop("No viewport supplied for `vplayoutheight' unit")
      else if (!is.layout(data[[i]]$layout))
        stop("No layout specified in viewport for `vplayoutheight' unit")
    }
  data
}

# NOTE: the result of match() is an integer, but subtracting 1 converts
# to real => have to convert back to integer for passing to C code
valid.units <- function(units) {
  original <- units
  int.units <- as.integer(match(units, .grid.unit.list) - 1)
  if (any(is.na(int.units)))
    stop(paste("Invalid units:", original))
  int.units
}

as.character.unit <- function(unit) {
  class(unit) <- NULL
  paste(unit, attr(unit, "unit"), sep="")
}

#########################
# UNIT ARITHMETIC STUFF
#########################

unit.arithmetic <- function(func.name, arg1, arg2=NULL) {
  ua <- list(fname=func.name, arg1=arg1, arg2=arg2)
  class(ua) <- c("unit.arithmetic", "unit")
  ua
}

Ops.unit <- function(x, y) {
  ok <- switch(.Generic, "+"=T, "-"=T, "*"=T, F)
  if (!ok)
    stop(paste("Operator", .Generic, "not meaningful for units"))
  if (.Generic=="*")
    # can only multiply a unit by a scalar
    if (nchar(.Method[1])) {
      if (nchar(.Method[2]))
        stop("Only one operand may be a unit")
      else if (is.numeric(y))
        # NOTE that we always put the scalar first
        unit.arithmetic(.Generic, y, x)
      else
        stop("Non-unit operand must be numeric")
    } else {
      if (is.numeric(x))
        unit.arithmetic(.Generic, x, y)
      else
        stop("Non-unit operand must be numeric")
    }
  else
    # Check that both arguments are units
    if (nchar(.Method[1]) && nchar(.Method[2]))
      unit.arithmetic(.Generic, x, y)
    else
      stop("Both operands must be units") 
}    

Summary.unit <- function(..., na.rm=FALSE) {
  # NOTE that this call to unit.c makes sure that arg1 is
  # a single unit object 
  x <- unit.c(...)
  ok <- switch(.Generic, "max"=T, "min"=T, "sum"=T, F)
  if (!ok)
    stop(paste("Summary function", .Generic, "not meaningful for units"))
  unit.arithmetic(.Generic, x)
}

is.unit.arithmetic <- function(x) {
  inherits(x, "unit.arithmetic")
}

as.character.unit.arithmetic <- function(ua) {
  # bit too customised for my liking, but whatever ...
  # NOTE that paste coerces arguments to mode character hence
  # this will recurse.
  fname <- ua$fname
  if (fname == "+" || fname == "-" || fname == "*")
    paste(ua$arg1, fname, ua$arg2, sep="")
  else
    paste(fname, "(", paste(ua$arg1, collapse=", "), ")", sep="")
}

#########################
# UNIT LISTS
# The idea with these is to allow arbitrary combinations
# of unit objects and unit arithmetic objects
#########################

# create a unit list from a unit, unit.arithmetic, or unit.list object
unit.list <- function(unit) {
  l <- unit.length(unit)
  result <- vector("list", l)
  for (i in 1:l)
    result[[i]] <- unit[i]
  class(result) <- c("unit.list", "unit")
  result
}

is.unit.list <- function(x) {
  inherits(x, "unit.list")
}
  
as.character.unit.list <- function(ul) {
  l <- unit.length(ul)
  result <- rep("", l)
  for (i in 1:unit.length(ul))
    result[i] <- as.character(ul[[i]])
  result
}

#########################
# These work on any sort of unit object
#########################

# FIXME:  I am doing my own dispatching here;  should be a generic function
is.unit <- function(unit) {
  inherits(unit, "unit")
}

print.unit <- function(unit) {
  print(as.character(unit), quote=F)
}

#########################
# Unit subsetting
#########################

# The idea of the "top" argument is to allow the function to
# know if it has been called from the command-line or from
# a previous (recursive) call to "[.unit" or "[.unit.arithmetic"
# this allows recycling beyond the end of the unit object
# except at the top level

# NOTE that "unit" and "data" attributes will be recycled
"[.unit" <- function(x, index, top=TRUE, ...) {
  this.length <- length(x)
  if (top && index > this.length)
    stop("Index out of bounds (unit subsetting)")
  cl <- class(x);
  units <- attr(x, "unit")
  valid.units <- attr(x, "valid.unit")
  data <- attr(x, "data")
  class(x) <- NULL;
  # The line below may seem slightly odd, but it should only be
  # used to recycle values when this method is called to
  # subset an argument in a unit.arithmetic object
  x <- x[(index - 1) %% this.length + 1]
  attr(x, "unit") <- units[(index - 1) %% length(units) + 1]
  attr(x, "valid.unit") <- valid.units[(index - 1) %% length(valid.units) + 1]
  # Need to handle vector index for list subsetting
  data.list <- list()
  for (i in index)
    data.list <- c(data.list, list(data[[(i - 1) %% length(data) + 1]]))
#  data.list <- vector("list", length(index))
#  count <- 1
#  for (i in index) {
#    data.list[[count]] <- data[[(i - 1) %% length(data) + 1]]
#    count <- count + 1
#  }
  attr(x, "data") <- data.list
  class(x) <- cl
  x
}

# NOTE that units will be recycled to the length of the largest
# of the arguments
"[.unit.arithmetic" <- function(x, index, top=TRUE, ...) {
  this.length <- unit.arithmetic.length(x)
  if (top && index > this.length)
    stop("Index out of bounds (unit arithmetic subsetting)")
  switch(x$fname,
         "+"="["(x$arg1, (index - 1) %% this.length + 1, top=FALSE) +
             "["(x$arg2, (index - 1) %% this.length + 1, top=FALSE),
         "-"="["(x$arg1, (index - 1) %% this.length + 1, top=FALSE) -
             "["(x$arg2, (index - 1) %% this.length + 1, top=FALSE),
         "*"=x$arg1 *
             "["(x$arg2, (index - 1) %% this.length + 1, top=FALSE),
         "min"=x,
         "max"=x,
         "sum"=x)
}

# Need to handle vector index for list subsetting
"[.unit.list" <- function(x, index, top=TRUE, ...) {
  this.length <- unit.list.length(x)
  if (top && index > this.length)
    stop("Index out of bounds (unit list subsetting)")
  cl <- class(x)
  result <- vector("list", length(index))
  count <- 1
  for (i in index) {
    result[[count]] <- x[[(i - 1) %% this.length + 1]]
    count <- count + 1
  }
  class(result) <- cl
  result
}

# Write "[<-.unit" methods too ?? 

#########################
# "c"ombining unit objects
#########################

# NOTE that I have not written methods for c()
# because method dispatch occurs on the first argument to
# "c" so c(unit(...), ...) would come here, but c(whatever, unit(...), ...)
# would go who-knows-where.
# A particularly nasty example is:  c(1, unit(1, "npc")) which will
# produce the same result as c(1, 1)
# Same problem for trying to control c(<unit>, <unit.arithmetic>)
# versus c(<unit.arithmetic>, <unit>), etc ...

# If any arguments are unit.arithmetic or unit.list, then the result will be
# unit.list
unit.c <- function(...) {
  x <- list(...)
  ual <- FALSE
  for (i in 1:length(x))
    if (inherits(x[[i]], "unit.list") ||
        inherits(x[[i]], "unit.arithmetic"))
      ual <- TRUE
  if (ual)
    unit.list.c(...)
  else {
    values <- NULL
    units <- NULL
    data <- NULL
    for (i in 1:length(x))
      if (is.unit(x[[i]])) {
        values <- c(values, x[[i]])
        units <- c(units, rep(attr(x[[i]], "unit"), length.out=length(x[[i]])))
        data <- c(data, recycle.data(attr(x[[i]], "data"), TRUE,
                                     length(x[[i]])))
      }
      else 
        stop("It is invalid to combine unit objects with other types")
    unit(values, units, data=data)
  }
}

unit.list.c <- function(...) {
  x <- list(...)
  result <- unit.list(x[[1]])
  i <- 2
  while (i < length(x) + 1) {
    result <- c(result, unit.list(x[[i]]))
    i <- i + 1
  }
  class(result) <- "unit.list"
  unit.list(result)
}

#########################
# Length of unit objects
#########################

unit.list.length <- function(ul) {
  length(ul)
}

# unit.length is designed to call this when appropriate
# so that this need never be called by the user
unit.arithmetic.length <- function(ua) {
  switch(ua$fname,
         "+"=max(unit.length(ua$arg1), unit.length(ua$arg2)),
         "-"=max(unit.length(ua$arg1), unit.length(ua$arg2)),
         "*"=unit.length(ua$arg2),
         "min"=1,
         "max"=1,
         "sum"=1)
}

# FIXME: I am doing my own dispatching here;  should be generic function
unit.length <- function(unit) {
  if (is.unit.list(unit))
    unit.list.length(unit)
  else if (is.unit.arithmetic(unit))
    unit.arithmetic.length(unit)
  else
    length(unit)
}
