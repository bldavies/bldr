#' Alphabetise data frame and tibble columns
#'
#' \code{sort_cols} alphabetises the columns of a data frame or tibble.
#'
#' @param .data A data frame or tibble.
#' @param ... (Optional) unquoted expressions determining left-most columns.
#' See \code{dplyr::select} for syntax requirements.
#'
#' @return An object of the same class as \code{.data}.
#'
#' @examples
#' sort_cols(mtcars, weight = wt)  # left-most column will be "weight"
#'
#' @export
sort_cols <- function(.data, ...) {
  dplyr::select(.data[, sort(names(.data))], ..., tidyselect::everything())
}
