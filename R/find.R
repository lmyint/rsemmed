#' Search for nodes by name or semantic type
#' 
#' Search for nodes by name (exact match or using regular expressions) 
#' or which match supplied semantic types. Perform anti-matching by 
#' setting \code{match = FALSE}. Capitalization is ignored.
#' 
#' @param obj Either the SemMed graph or a node set (\code{igraph.vs})
#' @param pattern Regular expression used to find matches in node names
#' @param names Character vector of exact node names
#' @param semtypes Character vector of semantic types
#' @param match If \code{TRUE}, return nodes that DO match \code{pattern}
#'              (default). If \code{FALSE}, return nodes that DO NOT match.
#' @return A vertex sequence of matching nodes
#' 
#' @examples
#' data(g_mini)
#' find_nodes(g_mini, pattern = "cortisol")
#' find_nodes(g_mini, pattern = "cortisol$")
#' find_nodes(g_mini, pattern = "stress")
#' find_nodes(g_mini, pattern = "stress") %>%
#'     find_nodes(pattern = "disorder", match = FALSE)
#' 
#' find_nodes(g_mini, names = "Serum cortisol")
#' find_nodes(g_mini, names = "Chronic Stress")
#' 
#' find_nodes(g_mini, semtypes = "dsyn")
#' find_nodes(g_mini, semtypes = c("dsyn", "fndg"))
#' 
#' ## pattern and semtypes are combined via OR:
#' find_nodes(g_mini, pattern = "cortisol", semtypes = "horm")
#' 
#' ## To make an AND query, chain find_nodes sequenctially:
#' find_nodes(g_mini, pattern = "cortisol") %>%
#'     find_nodes(semtypes = "horm")
#' 
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom igraph V
#' @export
find_nodes <- function(obj, pattern = NULL, 
                        names = NULL, semtypes = NULL, match = TRUE) {
    stopifnot(is(obj, "igraph") | is(obj, "igraph.vs"))
    if (is(obj, "igraph")) {
        obj <- igraph::V(obj)
    }

    ## Extract node features in lowercase
    node_names <- tolower(obj$name)
    node_semtypes <- tolower(obj$semtype)

    ## Name matching by regex
    if (is.null(pattern)) {
        bool_match_name_regex <- rep(FALSE, length(node_names))
    } else {
        pattern <- tolower(pattern)
        bool_match_name_regex <- stringr::str_detect(
            node_names, pattern = pattern)
    }

    ## Name matching by exact name
    if (is.null(names)) {
        bool_match_name_exact <- rep(FALSE, length(node_names))
    } else {
        names <- tolower(names)
        bool_match_name_exact <- node_names %in% names
    }

    ## Semantic type matching
    if (is.null(semtypes)) {
        bool_match_semtype <- rep(FALSE, length(node_names))
    } else {
        semtypes <- tolower(semtypes)
        bool_match_semtype <- .str_detect_pattern_vec(node_semtypes, semtypes)
    }

    ## Combine booleans
    bool_match <- bool_match_name_regex | 
                    bool_match_name_exact |
                    bool_match_semtype
    if (match) {
        obj[bool_match]
    } else {
        obj[!bool_match]
    }
}

#' Shortest paths between node sets
#' 
#' Find all shortest paths between sets of nodes
#' 
#' \code{find_paths} relies on \code{igraph::all_shortest_paths} to find all 
#' shortest paths between the nodes in \code{from} and \code{to}. This 
#' function searches for undirected paths.
#' 
#' Because the Semantic MEDLINE graph is a multigraph, there may be multiple 
#' paths with the same sequence of nodes. This function collapses these into 
#' a single node sequence. The display functions (\code{text_path} and
#' \code{plot_path}) take care of showing the multiple edges leading to 
#' repeated paths.
#' 
#' @param graph The SemMed graph
#' @param from A set of source nodes. \code{from} should be of class
#'             \code{igraph.vs} (a vertex sequence) or an integer vector.
#' @param to A set of destination nodes. \code{to} should be of class
#'           \code{igraph.vs} (a vertex sequence) or an integer vector.
#' @param weights A numeric vector of edge weights. If \code{NULL} 
#'                (the default), all edges have the default weight of 1.
#' @return A list of shortest paths. List items correspond to the 
#' node(s) given in \code{from}.
#' 
#' @seealso \code{\link{make_edge_weights}} to tailor the 
#' shortest path search
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, names = "Serum cortisol")
#' node_stress <- find_nodes(g_mini, names = "Chronic Stress")
#' find_paths(g_mini, from = node_cortisol, to = node_stress)
#' 
#' @importFrom igraph all_shortest_paths
#' @importFrom igraph as_ids
#' @export
find_paths <- function(graph, from, to, weights = NULL) {
    asp_list <- lapply(from, function(from_node) {
        asp <- igraph::all_shortest_paths(
            graph,
            from = from_node,
            to = to,
            mode = "all",
            weights = weights
        )
        asp$res
    })

    ## For each set of shortest paths...
    keep_list <- lapply(asp_list, function(path_list) {
        ## ... get the list of shortest paths as character vectors
        ##     of node names (rather than as igraph.vs)
        path_list_names <- lapply(path_list, igraph::as_ids)
        ## Get a logical vector of duplications
        is_repeat <- duplicated(path_list_names)
        !is_repeat
    })

    ## Apply the logicals to the original list
    lapply(seq_along(asp_list), function(i) {
        asp <- asp_list[[i]]
        keep <- keep_list[[i]]
        asp[keep]
    })
}
