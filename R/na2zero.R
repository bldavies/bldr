#' Replace NAs with zeros
#'
#' \code{na2zero} replaces NAs with zeros.
#'
#' @param x Input object.
#'
#' @examples
#' sapply(list(1, 2, NA, 4), na2zero)
#'
#' @export
na2zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
