#' Convert matrices to data frames
#'
#' \code{mat2df} converts a matrix \code{mat} to a data frame.
#'
#' @param mat Matrix to convert.
#' @param col.names Data frame column names.
#'   Defaults to \code{c("row", "col", "value")}.
#'
#' @seealso \code{\link{mat2tbl}}
#'
#' @export
mat2df <- function(mat, col.names = c("row", "col", "value")) {
  if (!is.character(col.names) | length(col.names) != 3) {
    stop("`col.names` must a be character vector of length three.")
  }
  df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(df) <- col.names
  df
}
