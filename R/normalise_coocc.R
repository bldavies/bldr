#' Normalise co-occurrence matrix
#'
#' \code{normalise_coocc} computes the normalisation of a co-occurrence
#' matrix \code{mat}.
#'
#' @param mat Co-occurrence matrix.
#'   Must be square.
#' @param method Normalisation method.
#'   The default is "jaccard".
#'   Other methods include "cosine" (same as "ochiai") and "dice".
#'
#' @references van Eck, N.J. and L. Waltman (2009).
#' How to normalize cooccurrence data?
#' An analysis of some well‐known similarity measures.
#' \emph{Journal of the American Society for Information Science and Technology}
#' 60(8):1635-1651.
#'
#' @export
normalise_coocc <- function(mat, method = "jaccard") {
  try(nrow(mat) == ncol(mat), stop("Co-occurrence matrix must be square"))
  C <- mat
  O <- matrix(rep(diag(C), nrow(C)), ncol = nrow(C))
  if (method == "jaccard") {
    C / (O + t(O) - C)
  } else if (method == "cosine" | method == "ochiai") {
    C / sqrt(O * t(O))
  } else if (method == "dice") {
    2 * C / (O + t(O))
  } else {
    stop("Unknown value for argument `method`")
  }
}
