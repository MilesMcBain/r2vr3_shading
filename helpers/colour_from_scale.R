##' Create colour scale and indexes for vector
##'
##' Given a vector of numeric values, return a palette of size `n_cols`` and a
##' set of indexes into the palette of size equal to the numeric vector.
##'
##' @title colour_from_scale
##' @param vec a numeric of vector of values to generate palette indexes for
##' @param palette_fn a function that will produce a vector of hex colour code
##'   strings, when called: `palette_fn(n_cols)`
##' @param n_cols the number of colours to generate using the `palette_fn`.
##' @param zero_index should the colour indexes begin at 0, for use with 0 indexed languages like Javascript?
##' @return A list contain two named vectors: 'colours' and 'indexes'.
colour_from_scale <- function(vec, palette_fn, n_cols, zero_index = FALSE){
  palette <- palette_fn(n_cols)
  palette_indexes <- ceiling(((vec - min(vec))/(max(vec) - min(vec))) * n_cols)
  palette_indexes[palette_indexes == 0] <- 1 ## adjust for values equal to the min

  if (zero_index) palette_indexes <- palette_indexes - 1

  list(colours = palette, indexes = palette_indexes)
}
