#' Replace NULLs with NAs
#'
#' \code{null2na} replaces NULLs with NAs.
#'
#' @param x Input object.
#'
#' @examples
#' sapply(list(1, 2, NULL, 4), null2na)
#'
#' @export
null2na <- function(x) {
  ifelse(is.null(x), NA, x)
}
