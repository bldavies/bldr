#' Convert decimal number of minutes to h:mm:ss representation
#'
#' \code{min2hms} converts a decimal number of minutes to a string
#'   representing the time in h:mm:ss format.
#'   h may exceed 23 and is omitted if h = 0.
#'
#' @param x Decimal number of minutes to be converted.
#' @param nearest Whether x should be rounded to the nearest second.
#'   Otherwise x is rounded down.
#'   Default is TRUE.
#'
#' @return A character scalar.
#'
#' @examples
#' min2hms(1624.145)  # 27:04:09
#'
#' @export
min2hms <- function(x, nearest = T) {
  sec2hms(x * 60, nearest)
}
