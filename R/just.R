# NOTE: the order of the strings in these conversion functions must
# match the order of the enums in ../src/lattice.h
# NOTE: the result of match() is an integer, but subtracting 1 converts
# to real => have to convert back to integer for passing to C code
valid.just <- function(just, n) {
  if (length(just) < n)
    just <- rep(just, length.out=n)
  just <- as.integer(match(just, c("left", "right", "bottom", "top",
                                   "centre", "center")) - 1)
  if (any(is.na(just)))
    stop("Invalid justification")
  just
}

justifyX <- function(x, width, just) {
  switch(just[1],
         left=x,
         centre=x - width*0.5,
         center=x - width*0.5,
         right=x - width)
}

justifyY <- function(y, height, just) {
  switch(if (length(just) > 1) just[2] else just[1],
         bottom=y,
         centre=y - height*0.5,
         center=y - height*0.5,
         top=y - height)
}


