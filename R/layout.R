
is.layout <- function(l) {
  inherits(l, "layout")
}

# FIXME:  The internal C code now does a lot of recycling of
# unit values, units, and data.  Can some/most/all of the
# recycling stuff below be removed ?
valid.layout <- function(nrow, ncol, widths, heights, respect) {
  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)
  # make sure we're dealing with a unit object
  if (!is.logical(respect)) {
    respect <- as.matrix(respect)
    if (!is.matrix(respect) || any(dim(respect) != c(nrow, ncol))) 
      stop("'respect' must be logical or an 'nrow' by 'ncol' matrix")
    }
  if (is.matrix(respect)) {
    respect.mat <- as.integer(respect)
    respect <- 2
  }
  else {
    respect.mat <- matrix(as.integer(0), nrow, ncol)
  }
  l <- list(nrow = nrow, ncol = ncol,
            widths = widths, heights = heights,
            respect = respect, valid.respect=as.integer(respect),
            respect.mat = respect.mat)
  class(l) <- "layout"
  l
}

layout.torture <- function() {
  # 1 = all relative widths and heights
  lshow.layout(llayout(3,2))
  # (1) with full respect
  lshow.layout(llayout(3,2, respect=T))
  # (1) with partial respect
  lshow.layout(llayout(3,2,respect=matrix(c(1,0,0,0,0,0), 3, 2, T)))
  # (1) with slightly weirder partial respect
  lshow.layout(llayout(3,2,respect=matrix(c(1,0,0,0,0,1), 3, 2, T)))
  # 2 = combination of absolute and relative widths and heights
  lshow.layout(llayout(2, 3,
                       widths=unit(c(2,4,1), c("null", "cm", "null")),
                       heights=unit(c(6,4), c("cm", "null"))))
  # (2) with full respect
  lshow.layout(llayout(2, 3, 
                       widths=unit(c(2,4,1), c("null", "cm", "null")),
                       heights=unit(c(6,4), c("cm", "null")), respect=T))
  # (2) with partial respect
  lshow.layout(llayout(2, 3, 
                       widths=unit(c(2,4,1), c("null", "cm", "null")),
                       heights=unit(c(6,4), c("cm", "null")),
                       respect=matrix(c(0,0,0,0,0,1), 2, 3, T)))
}

####################
# Accessors
####################

layout.nrow <- function(lay) {
  lay$nrow
}

layout.ncol <- function(lay) {
  lay$ncol
}

layout.widths <- function(lay) {
  lay$widths
}

layout.heights <- function(lay) {
  lay$heights
}

layout.respect <- function(lay) {
  switch(lay$respect + 1,
         FALSE,
         TRUE,
         lay$respect.mat)
}

####################
# Public constructor function
####################
grid.layout <- function (nrow = 1, ncol = 1,
                         widths = unit(rep(1, ncol), "null"), 
                         heights = unit(rep(1, nrow), "null"),
                         default.units = "null",
                         respect = FALSE)
{
  if (!is.unit(widths))
    unit(widths, default.units)
  if (!is.unit(heights))
    unit(heights, default.units) 
  valid.layout(nrow, ncol, widths, heights, respect)
}

####################
# Utility Functions
####################
