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


