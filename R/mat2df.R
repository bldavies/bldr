#' Convert matrices to data frames
#'
#' \code{mat2df} converts a matrix \code{mat} to a data frame.
#'   \code{mat2tbl} converts \code{mat} to a tibble.
#'
#' @param mat Matrix to convert.
#' @param col.names Output column names.
#'   Defaults to \code{c("row", "col", "value")}.
#'
#' @export
mat2df <- function(mat, col.names = c("row", "col", "value")) {
  if (!is.character(col.names) | length(col.names) != 3) {
    stop("`col.names` must a be character vector of length three.")
  }
  res <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(res) <- col.names
  res
}

#' @rdname mat2df
#' @export
mat2tbl <- function(mat, col.names = c("row", "col", "value")) {
  res <- mat2df(mat, col.names)
  dplyr::as_tibble(res)
}
