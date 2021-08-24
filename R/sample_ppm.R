#' Sample planted partition model
#'
#' \code{sample_ppm} generates a random graph using the planted partition model,
#'   which is a special case of the stochastic block model.
#'
#' @param memb Vector of community memberships.
#' @param p Probability that vertices in same community are adjacent.
#' @param q Probability that vertices in different communities are adjacent.
#' @param seed Initial seed for edge randomisation (optional).
#'
#' @return An igraph graph.
#'
#' @export
sample_ppm <- function(memb, p, q, seed = NULL) {
  mat <- t(utils::combn(seq_along(memb), 2))
  prob <- c(q, p)[1 + (memb[mat[, 1]] == memb[mat[, 2]])]
  if (!is.null(seed)) {
    set.seed(seed)
  }
  el <- mat[which(stats::runif(nrow(mat)) < prob), ]
  igraph::graph_from_data_frame(data.frame(el), directed = FALSE, vertices = seq_along(memb))
}
