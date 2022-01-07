#' Convert decimal times to h:mm:ss format
#'
#' \code{sec2hms} and \code{min2hms} convert decimal numbers of seconds and
#'   minutes to h:mm:ss format.
#'   h may exceed 23 and is omitted if h = 0.
#'
#' @param x Decimal number to be converted.
#' @param nearest Whether x should be rounded to the nearest second.
#'   Otherwise x is rounded down.
#'   Default is TRUE.
#'
#' @return A character scalar.
#'
#' @examples
#' sec2hms(97448.7)  # 27:04:09
#' min2hms(1624.145)  # 27:04:09
#'
#' @export
sec2hms <- function(x, nearest = T) {
  if (nearest) {
    x <- round(x)
  } else {
    x <- floor(x)
  }
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  if (nearest) {
    s <- x %% 60
  } else {
    s <- x %% 60
  }
  ifelse(h > 0, sprintf('%d:%.2d:%.2d', h, m, s), sprintf('%d:%.2d', m, s))
}

#' @rdname sec2hms
#' @export
min2hms <- function(x, nearest = T) {
  sec2hms(x * 60, nearest)
}
