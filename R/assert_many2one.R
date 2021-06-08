#' Assert that data contain many-to-one map between columns
#'
#' \code{assert_many2one} checks whether there is a many-to-one map between
#' two columns \code{x} and \code{y} of a data frame \code{.data}, raising an
#' error if not and returning \code{.data} otherwise.
#'
#' @param .data A data frame or tibble.
#' @param x,y Columns intended to share a many-to-one map.'=
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
