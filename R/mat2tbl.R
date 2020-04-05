#' Convert matrices to tibbles
#'
#' \code{mat2tbl} converts a matrix \code{mat} to a tibble.
#'
#' @param mat Matrix to convert.
#' @param col.names Tibble column names.
#'   Defaults to \code{c("row", "col", "value")}.
#'
#' @seealso \code{\link{mat2df}}
#'
#' @export
mat2tbl <- function(mat, col.names = c("row", "col", "value")) {
  df <- mat2df(mat, col.names = col.names)
  dplyr::as_tibble(df)
}
