#' Convert decimal number of seconds to h:mm:ss representation
#'
#' \code{sec2hms} converts a decimal number of seconds to a string
#'   representing the time in h:mm:ss format.
#'   h may exceed 23 and is omitted if h = 0.
#'
#' @param x Decimal number of seconds to be converted.
#' @param nearest Whether x should be rounded to the nearest second.
#'   Otherwise x is rounded down.
#'   Default is TRUE.
#'
#' @return A character scalar.
#'
#' @examples
#' sec2hms(97448.7)  # 27:04:09
#'
#' @export
sec2hms <- function(x, nearest = T) {
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  if (nearest) {
    s <- round(x) %% 60
  } else {
    s <- floor(x) %% 60
  }
  ifelse(h > 0, sprintf('%d:%.2d:%.2d', h, m, s), sprintf('%d:%.2d', m, s))
}
