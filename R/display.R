#' Display path (text form)
#' 
#' Show a text display of a path and obtain output that can
#' be used to explore predications along the path. (A predication 
#' is a SUBJECT--LINKING VERB-->OBJECT triple.)
#' 
#' \code{text_path} invisibly returns a list of \code{tbl}'s containing
#' information on the predications on the path. Each list element is a 
#' \code{tbl} that corresponds to a (sequential) pair of nodes along 
#' the path. The \code{tbl} contains information on the subject and 
#' object node's name and semantic type as well as all predicates linking 
#' the subject and object.
#' 
#' @param graph The SemMed graph
#' @param path A vertex sequence (\code{igraph.vs}) (the path to display)
#' @param print Print the path to screen?
#' @return Invisibly returns a list of predications for each pair
#' of nodes along the path.
#' 
#' @seealso \code{\link{plot_path}} for plotting paths
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, names = "Serum cortisol")
#' node_stress <- find_nodes(g_mini, names = "Chronic Stress")
#' paths <- find_paths(g_mini, from = node_cortisol, to = node_stress)
#' text_path(g_mini, paths[[1]][[1]])
#' result <- text_path(g_mini, paths[[1]][[1]], print = FALSE)
#' 
#' @importFrom igraph induced_subgraph
#' @importFrom igraph tail_of
#' @importFrom igraph head_of
#' @importFrom igraph edge_attr
#' @export
text_path <- function(graph, path, print = TRUE) {
    path_edge_feats <- .subgraph_features(graph, path, semtypes = TRUE)

    ## Loop over node pairs
    info_list <- vector("list", length(path)-1)
    for (i in seq_len(length(path)-1)) {
        node1 <- path[i]$name
        node2 <- path[i+1]$name
        path_edge_feats_subs <- path_edge_feats %>%
            dplyr::filter(
                (from==node1 & to==node2) | (to==node1 & from==node2)
            ) %>%
            dplyr::arrange(from, via)
        info_list[[i]] <- path_edge_feats_subs
        if (print) {
            cat(node1, "---", node2, ":\n")
            print(path_edge_feats_subs)
            cat("\n")
        }
    }
    invisible(info_list)
}

#' Display path (plot form)
#' 
#' Plot the graph form of a path
#' 
#' All connections among nodes along the supplied path are plotted with 
#' nodes labeled with their name and edges labeled with their predicate.
#' 
#' @param graph The SemMed graph
#' @param path A vertex sequence (\code{igraph.vs}) (the path to display)
#' @return A plot is created on the current graphics device
#' 
#' @seealso \code{\link{text_path}} for textual display of paths
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, names = "Serum cortisol")
#' node_stress <- find_nodes(g_mini, names = "Chronic Stress")
#' paths <- find_paths(g_mini, from = node_cortisol, to = node_stress)
#' plot_path(g_mini, paths[[1]][[1]])
#' 
#' @importFrom igraph induced_subgraph
#' @importFrom igraph edge_attr
#' @importFrom igraph curve_multiple
#' @export
plot_path <- function(graph, path) {
    g_subs <- igraph::induced_subgraph(graph, vids = path)
    plot(
        g_subs,
        edge.label = igraph::edge_attr(g_subs, "predicate"),
        curved = igraph::curve_multiple(g_subs, start = 0.05),
        edge.label.cex = 0.75,
        vertex.label.cex = 0.75
    )
}
