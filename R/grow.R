#' Obtain immediate neighbors
#' 
#' Grow a set of nodes into its first order neighborhood.
#' 
#' \code{grow_nodes} obtains the set of immediate neighbors of the
#' supplied nodes using \code{igraph::ego}. Unlike \code{ego}, 
#' \code{grow_nodes} flattens the result from a list to an ordinary
#' vertex sequence and removes the original search nodes.
#' 
#' @param graph The SemMed graph
#' @param nodes A vertex sequence (\code{igraph.vs}) of nodes to be grown
#' @return A vertex sequence of nodes in the neighborhood
#' (not including the original nodes)
#' 
#' @seealso \code{\link{find_nodes}} for filtering out irrelevant 
#' nodes from this set.
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, name = "hypercortisolemia")
#' nbrs <- grow_nodes(g_mini, node_cortisol)
#' 
#' @importFrom igraph ego
#' @importFrom igraph difference
#' @export
grow_nodes <- function(graph, nodes) {
    nbrs <- igraph::ego(graph, order = 1, nodes = nodes)
    nbrs <- unique(do.call(c, nbrs))
    added_nodes <- igraph::difference(nbrs, nodes)
    added_nodes
}
