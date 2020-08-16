#' Summarize semantic types
#' 
#' Summarize the semantic types present in a collection of nodes
#' 
#' \code{summarize_semtypes} summarizes the semantic types present in 
#' supplied node collections and has different behavior depending on 
#' whether the node collection is ordered (paths) or unordered. Using 
#' \code{is_path = TRUE} indicates that the nodes are ordered. Using 
#' \code{is_path = FALSE} indicates that the nodes are an unordered 
#' collection, often from \code{find_nodes} or \code{grow_nodes}.
#' 
#' Using \code{is_path = TRUE}: When the node collection is ordered, the 
#' object is assumed to be the result of \code{find_paths} or a subset of 
#' such an object. Because \code{find_paths} returns a list of paths lists, 
#' \code{summarize_semtypes} takes a single path, a list of paths, or a 
#' list of path lists as input. In the case of a collection of ordered nodes, 
#' \code{summarize_semtypes} counts the semantic types present in 
#' \code{object}. If a node is associated with multiple semantic types, 
#' each type is counted once. The first and last nodes of each path are 
#' removed they correspond to the nodes in \code{from} and \code{to} from 
#' \code{find_paths}, and it is assumed that the middle nodes on the paths 
#' are more of interest. The tabulations are printed to screen (if 
#' \code{print = TRUE}) and returned as \code{table}'s. These \code{table}'s 
#' are bundled into a list-column of a \code{tbl} in the (invisbly returned) 
#' output. Each row of the \code{tbl} corresponds to a \code{from}-\code{to} 
#' pair present in \code{object}.
#' 
#' Using \code{is_path = FALSE}: This option is for summarizing results from 
#' \code{find_nodes} and \code{grow_nodes}, which return unordered node sets.
#' (Note: paths and unordered node sets are both represented as \code{igraph} 
#' vertex sequences (class \code{igraph.vs}).) The printed output shows 
#' information for each semantic type present in \code{object}. It shows all 
#' nodes of that semantic type as well as their degree and degree percentile 
#' within the entire \code{graph}. The (invisibly returned) output combines 
#' all of the printed information in a \code{tbl}.
#' 
#' @param graph The SemMed graph
#' @param object A vertex sequence (\code{igraph.vs}), a list of vertex
#'               sequences, or a list of vertex sequence lists
#' @param print If \code{TRUE}, information on semantic types will be 
#'              printed to the screen.
#' @param is_path If \code{TRUE}, \code{object} contains paths (ordered 
#'                sequences of nodes).
#' @return Output is returned invisibly.
#' If \code{is_path = TRUE}, a \code{tbl} where each row corresponds 
#' to a \code{from}-\code{to} pair in \code{object}. The last column 
#' is a list-column containing \code{table}'s of semantic type counts.
#' If \code{is_path = FALSE}, a \code{tbl} where each row corresponds 
#' to a name-semantic type combination. Columns give node name, 
#' semantic type, degree, and degree percentile.
#' 
#' @seealso \code{\link{summarize_predicates}} for summarizing 
#' predicates on edges
#' @seealso \code{\link{find_paths}} for searching for paths 
#' between node sets
#' @seealso \code{\link{find_nodes}} and \code{\link{grow_nodes}} 
#' for searching for and filtering nodes
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, "Serum cortisol")
#' node_stress <- find_nodes(g_mini, "Chronic Stress")
#' paths <- find_paths(g_mini, from = node_cortisol, to = node_stress)
#' summarize_semtypes(g_mini, paths)
#' 
#' nodes_mood <- find_nodes(g_mini, "mood")
#' summarize_semtypes(g_mini, nodes_mood, is_path = FALSE)
#' 
#' @export
summarize_semtypes <- function(graph, object, print = TRUE, is_path = TRUE) {
    if (is_path) {
        df_paths <- .make_df_paths(graph, object, 
                                    by_from_to = TRUE, mark_middle = TRUE)

        ## Group by source-target pairs and tabulate
        df_summ <- dplyr::group_by(df_paths, from, to) %>%
            dplyr::summarize(semtypes = .table_sorted(semtype[middle])) %>%
            dplyr::arrange(from, to)

        if (print) .print_from_to_df(df_summ, "semtypes")
    } else {
        df_paths <- .make_df_paths(graph, object, 
                                    by_from_to = FALSE, mark_middle = FALSE)

        ## Get semtypes and repeat names to match semtype list structure
        semtypes <- stringr::str_split(df_paths$semtype, ",")
        node_names <- lapply(seq_along(semtypes), function(i) {
            rep(df_paths$name[i], length(semtypes[[i]]))
        })

        ## Put node names and semtypes in a data.frame & join degree info
        df_semtype_names <- dplyr::tibble(
            semtype = unlist(semtypes),
            name = unlist(node_names)
        )
        df_semtype_names <- df_semtype_names %>%
            dplyr::left_join(
                dplyr::select(df_paths, name, node_degree, node_degree_perc)
            )
        df_summ <- dplyr::group_by(df_semtype_names, semtype) %>%
            dplyr::mutate(max_degree = max(node_degree)) %>%
            dplyr::arrange(
                dplyr::desc(max_degree), semtype, dplyr::desc(node_degree)
            ) %>%
            dplyr::select(-max_degree)

        if (print) {
            unique_semtypes <- unique(df_summ$semtype)
            for (st in unique_semtypes) {
                df_summ_subs <- df_summ %>% dplyr::filter(semtype==st) %>%
                    select(-semtype)
                cat(st, "--------------------\n")
                print(df_summ_subs)
                cat("\n")
            }
        }
    }
    invisible(df_summ)
}

#' Summarize predicates
#' 
#' Summarize the predicates present in a collection of paths
#' 
#' Because predicates are edge features, it is assumed that by using
#' \code{summarize_predicates} the nodes contained in \code{object} are 
#' ordered (paths). This is why \code{summarize_semtypes} has the 
#' \code{is_path} argument, but \code{summarize_predicates} does not.
#' \code{summarize_predicates} tabulates edge predicates across paths 
#' corresponding to each \code{from}-\code{to} pair in \code{object}.
#' 
#' @param graph The SemMed graph
#' @param object A vertex sequence (\code{igraph.vs}), a list of vertex
#'               sequences, or a list of vertex sequence lists
#' @param print If \code{TRUE}, information on predicates will be 
#'              printed to the screen.
#' @return A \code{tbl} where each row corresponds to a 
#' \code{from}-\code{to} pair in \code{object}. The last column is a
#' list-column containing \code{table}'s of predicate counts.
#' 
#' @seealso \code{\link{summarize_semtypes}} for tabulating 
#' semantic types of nodes in paths or other node collections
#' 
#' @examples
#' data(g_mini)
#' 
#' node_cortisol <- find_nodes(g_mini, "Serum cortisol")
#' node_stress <- find_nodes(g_mini, "Chronic Stress")
#' paths <- find_paths(g_mini, from = node_cortisol, to = node_stress)
#' summarize_predicates(g_mini, paths)
#' 
#' @export
summarize_predicates <- function(graph, object, print = TRUE) {
    df_paths <- .make_df_paths(graph, object, 
                                by_from_to = TRUE, mark_middle = FALSE)

    ## Keep needed columns
    df_paths <- df_paths %>%
        dplyr::select(from, to, node_id, name, grand_path_id, 
            node_index, path_length)

    ## Get subgraph and its edge features
    df_pred <- .subgraph_features(graph, 
                    vids = unique(df_paths$node_id), semtypes = FALSE)

    ## Reshape dataset: rows correspond to node pairs instead of nodes
    ## Need one col for node 1 in pair and one col for node 2
    rows_not_last_node <- which(df_paths$node_index != df_paths$path_length)
    rows_not_first_node <- which(df_paths$node_index != 1)
    stopifnot(length(rows_not_last_node)==length(rows_not_first_node))
    df_paths <- dplyr::tibble(
        from = df_paths$from[rows_not_last_node],
        to = df_paths$to[rows_not_last_node],
        grand_path_id = df_paths$grand_path_id[rows_not_last_node],
        name1 = df_paths$name[rows_not_last_node],
        name2 = df_paths$name[rows_not_first_node]
    )

    ## Merge in "forward" predicates: from --> to
    df_paths_forward <- .merge_predicates(df_paths, df_pred, "forward")
    ## Merge in "reverse" predicates: to --> from
    df_paths_reverse <- .merge_predicates(df_paths, df_pred, "reverse")
    ## Merge all predicates from "forward" and "reverse"
    df_final <- dplyr::full_join(df_paths_forward, df_paths_reverse)

    ## Concatenate (without NAs) "via_forward" and "via_reverse"
    df_final$predicates <- mapply(.combine_drop_na,
        df_final$via_forward, df_final$via_reverse
    )

    ## Tabulate predicates across from-to pairs
    df_final <- df_final %>%
        dplyr::group_by(from, to) %>%
        dplyr::summarize(predicates = .table_sorted_from_list(predicates))

    if (print) .print_from_to_df(df_final, "predicates")

    invisible(df_final)
}
