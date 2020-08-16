utils::globalVariables(c("from", "grand_path_id", 
    "max_degree", "middle", "name", "name1", "name2", "node_degree", 
    "node_degree_perc", "node_id", "node_index", "path_group", "path_id", 
    "path_length", "predicates", "semtype", "to", "vertex_attr", "via",
    "from_semtype", "to_semtype"))

#' Get information about edges
#' 
#' Search for nodes by name using regular expressions or which match given 
#' semantic types. Perform anti-matching by setting \code{match = FALSE}.
#' 
#' @param graph The SemMed graph
#' @param include_degree If \code{TRUE}, include information on head/tail 
#'                       node degrees.
#' @param include_node_ids If \code{TRUE}, include the ID numbers of 
#'                         head/tail nodes.
#' @param include_num_instances If \code{TRUE}, include information on the
#'                              number of times a predication was observed 
#'                              in the Semantic MEDLINE database.
#' @return A \code{tbl} where each row corresponds to an edge in the 
#' Semantic MEDLINE graph. The ordering of the rows corresponds to
#' \code{E(graph)}. Features (columns) always returned include the 
#' name and semantic type of the head (subject) and tail (object) nodes.
#' 
#' @seealso \code{\link{make_edge_weights}} for using this data to 
#' construct edge weights
#' 
#' @examples
#' data(g_mini)
#' 
#' e_feat <- get_edge_features(g_mini)
#' 
#' @import dplyr
#' @importFrom igraph gsize
#' @importFrom igraph set_vertex_attr
#' @importFrom igraph V
#' @importFrom igraph vertex_attr
#' @importFrom igraph tail_of
#' @importFrom igraph head_of
#' @importFrom igraph edge_attr
#' @export
get_edge_features <- function(graph, include_degree = FALSE, 
    include_node_ids = FALSE, include_num_instances = FALSE) {
    num_edges <- igraph::gsize(graph)

    ## Turn names to lowercase, get edge head/tail, get degree stats
    graph <- .graph_lowercase(graph)
    nodes_subj <- igraph::tail_of(graph, es = seq_len(num_edges))
    nodes_obj <- igraph::head_of(graph, es = seq_len(num_edges))
    node_degree_stats <- .node_degree_stats(graph)

    feat <- dplyr::tibble(
        node_subj_name = nodes_subj$name,
        node_obj_name = nodes_obj$name,
        node_subj_semtypes = nodes_subj$semtype,
        node_obj_semtypes = nodes_obj$semtype,
        edge_pred = igraph::edge_attr(graph, name = "predicate")
    )

    ## Turn edge features to lowercase (semtypes, names already lowercase)
    feat$edge_pred <- tolower(feat$edge_pred)

    if (include_node_ids) {
        feat <- feat %>%
            dplyr::mutate(
                node_subj_id = as.integer(nodes_subj),
                node_obj_id = as.integer(nodes_obj)
            )
    }

    if (include_degree) {
        node_subj_degree_stats <- .rename_degree_df(node_degree_stats, "subj")
        node_obj_degree_stats <- .rename_degree_df(node_degree_stats, "obj")
        feat <- feat %>%
            dplyr::left_join(node_subj_degree_stats) %>%
            dplyr::left_join(node_obj_degree_stats)
    }

    if (include_num_instances) {
        feat$num_instances <- igraph::edge_attr(graph, name = "num_instances")
    }

    feat %>%
        dplyr::select(
            dplyr::starts_with("node_subj"),
            dplyr::starts_with("node_obj"),
            dplyr::everything()
        )
}

#' Create edge weights
#' 
#' Create edge weights to modify the shortest path search 
#' (\code{find_paths}). Discourage and/or encourage certain types of paths 
#' by supplying \code{_out} and \code{_in} arguments, respectively. Node 
#' semantic types, node names, and edge predicates are the features that 
#' can influence the edge weights. Capitalization is ignored.
#' 
#' @param graph The SemMed graph
#' @param e_feat A \code{data.frame} of edge features from 
#'               \code{get_edge_features}.
#' @param node_semtypes_out A character vector of semantic types to exclude 
#'                          from shortest paths.
#' @param node_names_out A character vector of exact node names to exclude.
#' @param edge_preds_out A character vector of edge predicates to exclude.
#' @param node_semtypes_in A character vector of semantic types to 
#'                         include/encourage in shortest paths.
#' @param node_names_in A character vector of exact node names to include.
#' @param edge_preds_in A character vector of edge predicates to include.
#' @return A numeric vector of weights
#' 
#' @seealso \code{\link{find_paths}}, \code{\link{get_middle_nodes}} for a 
#' way to obtain node names to remove
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, names = "Serum cortisol")
#' node_stress <- find_nodes(g_mini, names = "Chronic Stress")
#' paths <- find_paths(g_mini, from = node_cortisol, to = node_stress)
#' 
#' e_feat <- get_edge_features(g_mini)
#' 
#' w1 <- make_edge_weights(g_mini, e_feat, edge_preds_in = "COEXISTS_WITH")
#' paths1 <- find_paths(g_mini,
#'     from = node_cortisol, to = node_stress, weights = w1)
#' 
#' w2 <- make_edge_weights(g_mini, e_feat, edge_preds_in = "ISA",
#'                         node_names_out = "Stress")
#' paths2 <- find_paths(g_mini,
#'     from = node_cortisol, to = node_stress, weights = w2)
#' 
#' @importFrom igraph gsize
#' @importFrom igraph vcount
#' @export
make_edge_weights <- function(graph, e_feat,
    node_semtypes_out = NULL, node_names_out = NULL, edge_preds_out = NULL,
    node_semtypes_in = NULL, node_names_in = NULL, edge_preds_in = NULL) {
    .check_edge_features(e_feat)

    num_edges <- igraph::gsize(graph)
    num_nodes <- igraph::vcount(graph)

    ## Initialize vector of edge weights
    w <- rep(1, num_edges)

    ## First: by node semantic type. _out then _in
    bool_sem_out <- .weights_mark_semtype(node_semtypes_out, e_feat)
    bool_sem_in <- .weights_mark_semtype(node_semtypes_in, e_feat)

    ## Second: by node name. _out then _in
    bool_name_out <- .weights_mark_name(node_names_out, e_feat)
    bool_name_in <- .weights_mark_name(node_names_in, e_feat)

    ## Third: by edge predicate
    bool_pred_out <- .weights_mark_pred(edge_preds_out, e_feat)
    bool_pred_in <- .weights_mark_pred(edge_preds_in, e_feat)

    ## Combine booleans
    bool_high_weight <- bool_sem_out | bool_name_out | bool_pred_out
    bool_low_weight <- bool_sem_in | bool_name_in | bool_pred_in

    ## If any edges have a low weight, give all edges 1/num_nodes weight
    ## at first. Otherwise only giving 1/num_nodes weight to "in" edges
    ## won't incentivize them as desired.
    low_weight <- 1/num_nodes
    lowest_weight <- 0.5*low_weight
    if (any(bool_low_weight)) {
        w <- rep(low_weight, num_edges)
    }
    ## Give "in"-marked edges lower weight
    w[bool_low_weight] <- lowest_weight
    
    ## Give "out"-marked edges the maximum weight
    ## Due to semtype overlap, there might be edges marked for both
    ## low and high weight. Default to high weight.
    w[bool_high_weight] <- num_nodes
    
    w
}

#' Obtain the middle nodes of a path
#' 
#' For each pair of source and target nodes in \code{object}, obtain the 
#' names of middle nodes on paths.
#' 
#' @param graph The SemMed graph
#' @param object A vertex sequence (\code{igraph.vs}), a list of vertex
#'               sequences, or a list of vertex sequence lists
#' @param collapse If \code{TRUE}, middle node names for different 
#'                 source-target pairs are combined into one character 
#'                 vector.
#' @return A \code{tbl} where each row corresponds to a source-target pair 
#' in \code{object}. The last column is a list-column containing character 
#' vectors of names of middle nodes.
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, "Serum cortisol")
#' node_stress <- find_nodes(g_mini, "Chronic Stress")
#' paths <- find_paths(g_mini, from = node_cortisol, to = node_stress)
#' middle <- get_middle_nodes(g_mini, paths)
#' 
#' @export
get_middle_nodes <- function(graph, object, collapse = TRUE) {
    df_paths <- .make_df_paths(graph, object, 
                                by_from_to = TRUE, mark_middle = TRUE)

    result <- df_paths %>%
        dplyr::group_by(from, to) %>%
        dplyr::summarize(middle_nodes = .unique_as_list(name[middle]))

    if (collapse) {
        unlist(result$middle_nodes)
    } else {
        result
    }
}
