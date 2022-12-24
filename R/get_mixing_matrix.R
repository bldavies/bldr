#' Compute mixing matrix of a graph
#'
#' \code{get_mixing_matrix} returns the mixing matrix of a graph \code{G} based
#'   on a node attribute \code{att}.
#'
#' @param G An igraph graph.
#' @param att An attribute name.
#' @param counts Whether to return edge counts rather than proportions.
#'   Defaults to FALSE.
#'   If TRUE then returns matrix defined by Equation (1) in Newman (2003).
#'
#' @return A matrix.
#'
#' @references Newman, M. E. J, (2003).
#' Mixing patterns in networks.
#' \emph{Physical Review E}
#' 67:026126.
#' \doi{10.1103/PhysRevE.67.026126}
#'
#' @export
get_mixing_matrix <- function(G, att, counts = FALSE) {

  if (!att %in% names(igraph::vertex.attributes(G))) {
    stop(paste0("Input graph has no attribute \"", att, "\""))
  }

  memb <- igraph::get.vertex.attribute(G, att)

  if (sum(is.na(memb)) > 0) {
    warning(sprintf("Attribute \"%s\" missing for %d nodes", att, sum(is.na(memb))))
  }

  el <- igraph::as_edgelist(G, names = FALSE)

  res <- table(memb[el[, 1]], memb[el[, 2]])

  if (!igraph::is.directed(G)) {
    res <- res + t(res)
  }

  if (counts) {
    return(res)
  } else {
    return(res / sum(res))
  }

}
