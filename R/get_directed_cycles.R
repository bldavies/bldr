#' Find unique cycles in directed graph
#'
#' \code{get_directed_cycles} lists the cycles in a directed graph \code{G}.
#'
#' @param G An igraph graph.
#'   Must be directed.
#'
#' @return A list of directed cycles.
#'
#' @export
get_directed_cycles <- function(G) {

  if (!igraph::is.directed(G)) {
    stop('G must be directed')
  }

  func <- function(l, f, ...) {
    unlist(lapply(l, f, ...), recursive = F)
  }

  n <- igraph::gorder(G)

  cycles <- func(1:n, function(v) {
    func(igraph::neighbors(G, v, 'in'), function(x) {
      igraph::all_simple_paths(G, v, x)
    })
  })

  res <- lapply(cycles, function(x) {
    x <- as.vector(x)
    k <- which.min(x)
    if (k == 1) {
      x
    } else {
      c(x[-(1:(k - 1))], x[1:(k - 1)])
    }
  })

  unique(res)

}
