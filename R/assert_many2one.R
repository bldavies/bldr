#' Assert that data contain many-to-one or one-to-one map between columns
#'
#' \code{assert_many2one} checks whether there is a many-to-one map between
#'   two columns \code{x} and \code{y} of a data frame \code{.data}, raising an
#'   error if not and returning \code{.data} otherwise.
#'   \code{assert_one2one} checks for a one-to-one map.
#'   Both functions are intended for use in pipes.
#'
#' @param .data A data frame or tibble.
#' @param x,y Columns intended to share a many-to-one or one-to-one map.
#' @param na.rm Should NA values be ignored?
#'   Default is TRUE.
#'
#' @export
assert_many2one <- function(.data, x, y, na.rm = T) {
  xx <- dplyr::pull(.data, {{ x }})
  yy <- dplyr::pull(.data, {{ y }})
  if (!is_many2one(xx, yy, na.rm)) {
    stop('Correspondence is not m:1')
  }
  .data
}

#' @rdname assert_many2one
#' @export
assert_one2one <- function(.data, x, y, na.rm = T) {
  xx <- dplyr::pull(.data, {{ x }})
  yy <- dplyr::pull(.data, {{ y }})
  if (!is_one2one(xx, yy, na.rm)) {
    stop('Correspondence is not 1:1')
  }
  .data
}
