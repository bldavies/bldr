#' Test for many-to-one correspondence between vectors
#'
#' \code{is_many2one} tests whether there is a many-to-one map from the values
#' in a vector \code{x} to the corresponding values in a vector \code{y}.
#'
#' @param x,y Vectors of equal length.
#' @param na.rm Should NA values be ignored?
#'   Default is TRUE.
#'
#' @export
is_many2one <- function(x, y, na.rm = T) {

  if (length(x) != length(y)) {
    stop('x and y must have same length')
  }

  if (na.rm) {
    keep <- !(is.na(x) | is.na(y))
    x <- x[keep]
    y <- y[keep]
  }

  if (length(x) == 0) {
    stop('Correspondence must be non-trivial')
  }

  u <- unique(x)
  n <- sapply(seq_along(u), function(i) length(unique(y[which(x == u[i])])))

  return (max(n) == 1)
}