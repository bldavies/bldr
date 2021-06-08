#' Assert that data contain many-to-one map between columns
#'
#' \code{assert_many2one} checks whether there is a many-to-one map between
#' two columns \code{x} and \code{y} of a data frame \code{.data}, raising an
#' error if not and returning \code{.data} otherwise.
#'
#' @param .data A data frame or tibble.
#' @param x,y Columns intended to share a many-to-one map.
#'
#' @export
assert_many2one <- function(.data, x, y) {
  d <- dplyr::filter(.data, !is.na({{ x }}) & !is.na({{ y }}))
  d <- dplyr::distinct(d, {{ x }}, {{ y }})
  t <- max(dplyr::count(d, {{ x }})$n)
  if (t > 1) {
    stop('Correspondence is not m:1')
  }
  .data
}
