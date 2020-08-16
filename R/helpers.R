######################################################################
## Helpers for get_edge_features()
######################################################################
.rename_degree_df <- function(data, node_type) {
    name_new <- paste0("node_", node_type, "_name")
    degree_new <- paste0("node_", node_type, "_degree")
    degree_perc_new <- paste0("node_", node_type, "_degree_perc")
    colnames(data)[colnames(data)=="name"] <- name_new
    colnames(data)[colnames(data)=="node_degree"] <- degree_new
    colnames(data)[colnames(data)=="node_degree_perc"] <- degree_perc_new
    data
}



######################################################################
## Helpers for make_edge_weights()
######################################################################
.check_edge_features <- function(e_feat) {
    stopifnot(is(e_feat, "data.frame"))
    needed_colnames <- c(
        "node_subj_semtypes", "node_obj_semtypes",
        "node_subj_name", "node_obj_name", "edge_pred"
    )
    has_correct_colnames <- needed_colnames %in% colnames(e_feat)
    stopifnot(has_correct_colnames)
}

.weights_mark_semtype <- function(arg, e_feat) {
    num_edges <- nrow(e_feat)

    if (is.null(arg)) {
        bool <- rep(FALSE, num_edges)
    } else {
        arg <- tolower(arg)
        bool_subj <- .str_detect_pattern_vec(e_feat$node_subj_semtypes, arg)
        bool_obj <- .str_detect_pattern_vec(e_feat$node_obj_semtypes, arg)
        bool <- bool_subj | bool_obj
    }
    bool
}

.weights_mark_name <- function(arg, e_feat) {
    num_edges <- nrow(e_feat)

    if (is.null(arg)) {
        bool <- rep(FALSE, num_edges)
    } else {
        arg <- tolower(arg)
        bool_subj <- e_feat$node_subj_name %in% arg
        bool_obj <- e_feat$node_obj_name %in% arg
        bool <- bool_subj | bool_obj
    }
    bool
}

.weights_mark_pred <- function(arg, e_feat) {
    num_edges <- nrow(e_feat)

    if (is.null(arg)) {
        bool <- rep(FALSE, num_edges)
    } else {
        arg <- tolower(arg)
        bool <- e_feat$edge_pred %in% arg
    }
    bool
}


######################################################################
## Helpers for summarize_predicates()
######################################################################
## Merge directional predicates into a paths dataset
.merge_predicates <- function(df_paths, df_pred, 
                        direction = c("forward", "reverse")) {
    direction <- match.arg(direction)
    ## Create a long dataset
    if (direction=="forward") {
        df_paths_direction <- df_paths %>%
            dplyr::left_join(df_pred, by = c("name1" = "from", "name2" = "to"))
    } else {
        df_paths_direction <- df_paths %>%
            dplyr::left_join(df_pred, by = c("name2" = "from", "name1" = "to"))
    }

    ## Reshape to collapse predicates ("via") into a list-column
    df_paths_direction <- df_paths_direction %>%
        dplyr::group_by(from, to, grand_path_id, name1, name2) %>%
        dplyr::summarize(via_direction = list(via))
    cnames <- colnames(df_paths_direction)
    cnames[cnames=="via_direction"] <- paste0("via_", direction)
    colnames(df_paths_direction) <- cnames

    df_paths_direction
}



######################################################################
## Miscellaneous helpers
######################################################################
## Detect any of a set of patterns in a vector
.str_detect_pattern_vec <- function(s, pattern_vec) {
    pattern <- paste(pattern_vec, collapse = "|")
    stringr::str_detect(s, pattern)
}

## Determine nesting level of object
## 
## Is the object an \code{igraph.vs}, list of those, or
## list of list of those?
.object_type <- function(object) {
    is_lvl1_vs <- is(object, "igraph.vs")
    is_lvl1_list <- is(object, "list")
    is_lvl2_vs <- is(object[[1]], "igraph.vs")
    is_lvl2_list <- is(object[[1]], "list")
    is_lvl3_vs <- is(object[[1]][[1]], "igraph.vs")
    if (is_lvl1_vs) {
        type <- "vs"
    } else if (is_lvl1_list & is_lvl2_vs) {
        type <- "list_1_vs"
    } else if (is_lvl1_list & is_lvl2_list & is_lvl3_vs) {
        type <- "list_2_vs"
    } else {
        warning(
            "object does not seem to be of class igraph.vs,
            a result of igraph::all_shortest_paths(),
            or find_paths()"
        )
    }
    type
}

## Convert path list into a data frame
.paths_list_as_df <- function(graph, object) {
    ## igraph indices for nodes
    node_idx <- unlist(object)

    ## Path group (1st level index)
    path_group <- lapply(seq_along(object), function(i) {
        path_lengths <- lengths(object[[i]])
        rep(i, sum(path_lengths))
    })
    path_group <- unlist(path_group)

    ## Path ID within group (2nd level index)
    path_id <- lapply(object, function(vs_list) {
        rep(seq_along(vs_list), times = lengths(vs_list))
    })
    path_id <- unlist(path_id)

    ## Index of node within each path (3rd level index)
    path_idx <- lapply(object, function(vs_list) {
        lapply(vs_list, function(vs) {
            seq_along(vs)
        })
    })
    path_idx <- unlist(path_idx)

    ## Length of each path
    path_length <- lapply(object, function(vs_list) {
        lapply(vs_list, function(vs) {
            len <- length(vs)
            rep(len, len)
        })
    })
    path_length <- unlist(path_length)

    res <- dplyr::tibble(
        node_id = node_idx,
        name = igraph::vertex_attr(graph, "name", node_idx),
        semtype = igraph::vertex_attr(graph, "semtype", node_idx),
        path_group = path_group,
        path_id = path_id,
        grand_path_id = paste0(path_group, "_", path_id),
        node_index = path_idx,
        path_length = path_length
    )
    node_degree_stats <- .node_degree_stats(graph)
    res %>%
        dplyr::left_join(node_degree_stats)
}

## Create and process a path data frame
## 
## Convert a path object to a data frame and perform optional 
## additional processing
## 
## @param graph The SemMed graph
## @param object A path, list of paths, or list of list of paths
## @param by_from_to Logical. Organize information by from-to pairs?
## @param mark_middle Logical. Indicate middle nodes of paths?
.make_df_paths <- function(graph, object, by_from_to, mark_middle) {
    ## Determine object type
    type <- .object_type(object)

    ## Create path information data.frame
    if (type=="vs") {
        df_paths <- .paths_list_as_df(graph, list(list(object)))
    } else if (type=="list_1_vs") {
        df_paths <- .paths_list_as_df(graph, list(object))
    } else if (type=="list_2_vs") {
        df_paths <- .paths_list_as_df(graph, object)
    }

    if (by_from_to) {
        ## Get source and target node for each path
        df_paths_from_to <- df_paths %>%
            dplyr::group_by(path_group, path_id) %>%
            dplyr::summarize(
                from = name[node_index==1],
                to = name[node_index==path_length]
            )

        ## Add source and target info back in
        df_paths <- df_paths %>%
            dplyr::left_join(df_paths_from_to)
    }
    if (mark_middle) {
        ## Indicate middle nodes
        df_paths <- df_paths %>%
            dplyr::mutate(middle = node_index != 1 & node_index != path_length)
    }
    df_paths
}

## Make graph names lowercase
.graph_lowercase <- function(graph) {
    igraph::set_vertex_attr(
        graph = graph,
        name = "name", 
        index = igraph::V(graph), 
        value = tolower(igraph::vertex_attr(graph, "name"))
    )
}

## Turn a numeric vector into percentiles
.percentile <- function(x) {
    stopifnot(is(x, "numeric") | is(x, "integer"))
    rank(x, ties.method = "min")*100/length(x)
}

## Get degree information from a graph
## 
## Get information on node degree and percentile of node degree.
## 100th percentile = largest degree
.node_degree_stats <- function(graph) {
    nodes <- igraph::V(graph)
    dplyr::tibble(
        name = nodes$name,
        node_degree = igraph::degree(graph, nodes),
        node_degree_perc = .percentile(node_degree)
    )
}

## Get edge features from induced subgraph
## 
## @param graph The SemMed graph or subgraph
## @param vids Vertex IDs to subset to
## @param semtypes Include semantic type information?
## @return A \code{data.frame} of edge features, one row per edge
.subgraph_features <- function(graph, vids, semtypes = TRUE) {
    ## Get subgraph formed by the supplied nodes
    g_subs <- igraph::induced_subgraph(graph, vids = vids)

    ## Get features of edges in this subgraph:
    ## start and end nodes, predicates
    edge_start_nodes <- igraph::tail_of(g_subs, igraph::E(g_subs))
    edge_end_nodes <- igraph::head_of(g_subs, igraph::E(g_subs))
    edge_predicates <- igraph::edge_attr(g_subs, "predicate")
    feats <- dplyr::tibble(
        from = edge_start_nodes$name,
        via = edge_predicates,
        to = edge_end_nodes$name
    )
    if (semtypes) {
        feats$from_semtype <- edge_start_nodes$semtype
        feats$to_semtype <- edge_end_nodes$semtype
        feats <- feats %>%
            select(from_semtype, from, via, to, to_semtype)
    }

    feats
}

## Concatenate two vectors and drop NAs
.combine_drop_na <- function(x1, x2) {
    result <- c(x1,x2)
    result[!is.na(result)]
}

## Print information about from-to pairs
## 
## Print information about a certain attibute of from-to pairs
## 
## \code{data} should contain a \code{from} and \code{to} column. It 
## should also contain another column indicated by \code{which_col}.
## \code{which_col} is the main feature of interest to be printed.
## 
## @param data A \code{data.frame} with columns named \code{from} and 
##             \code{to}.
.print_from_to_df <- function(data, which_col) {
    for (i in seq_len(nrow(data))) {
        cat(data$from[i], "---------->", data$to[i], "\n")
        print(data[[which_col]][[i]])
        cat("\n")
    }
}
