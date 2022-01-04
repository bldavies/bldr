#' Test for one-to-one correspondence between vectors
#'
#' \code{is_one2one} tests whether there is a one-to-one map from the values
#' in a vector \code{x} to the corresponding values in a vector \code{y}.
#'
#' @param x,y Vectors of equal length.
#' @param na.rm Should NA values be ignored?
#'   Default is TRUE.
#'
#' @export
is_one2one <- function(x, y, na.rm = T) {
  is_many2one(x, y, na.rm) & is_many2one(y, x, na.rm)
}
