## code to prepare `g_small` dataset goes here

library(tidyverse)
library(igraph)

load("~/GoogleDriveMac/research/public_data/SemMedDB/g.rda")
source("R/utils.R")
source("R/display.R")
source("R/find.R")
source("R/grow.R")
source("R/summarize.R")

format(object.size(g), units = "Mb")

nodes_sickle <- find_nodes(g, pattern = "sickle")
nodes_sickle_subs <- nodes_sickle[c(2,4,9)]
nodes_malaria <- find_nodes(g, pattern = "malaria")
nodes_malaria_subs <- nodes_malaria %>%
    find_nodes(pattern = "anti", match = FALSE) %>%
    find_nodes(pattern = "test", match = FALSE) %>%
    find_nodes(pattern = "screening", match = FALSE) %>%
    find_nodes(pattern = "pigment", match = FALSE) %>%
    find_nodes(pattern = "smear", match = FALSE) %>%
    find_nodes(pattern = "parasite", match = FALSE) %>%
    find_nodes(pattern = "serology", match = FALSE) %>%
    find_nodes(pattern = "vaccine", match = FALSE)

paths <- find_paths(graph = g, from = nodes_sickle_subs, to = nodes_malaria_subs)

nodes <- do.call(c, do.call(c, paths)) %>% unique()
nodes <- c(nodes, nodes_sickle_subs, nodes_malaria) %>% unique()

e_feat <- get_edge_features(g)

## Create weights
w <- make_edge_weights(g, e_feat, node_semtypes_out = c("humn", "popg"))
paths_subset_reweight1 <- find_paths(
    graph = g,
    from = find_nodes(g, pattern = "sickle trait"),
    to = find_nodes(g, pattern = "Malaria, Cerebral"),
    weights = w
)
paths_subset_reweight1 <- paths_subset_reweight1[[1]]
nodes1 <- do.call(c, paths_subset_reweight1) %>% unique()

## Obtain the middle nodes (2nd node on the path)
paths_subset <- find_paths(
    graph = g,
    from = find_nodes(g, pattern = "sickle trait"),
    to = find_nodes(g, pattern = "Malaria, Cerebral")
)
paths_subset <- paths_subset[[1]]
out_names <- get_middle_nodes(g, paths_subset)
## Readjust weights
w <- make_edge_weights(g, e_feat, node_names_out = out_names, node_semtypes_out = c("humn", "popg"))
## Find paths with new weights
paths_subset_reweight2 <- find_paths(
    graph = g,
    from = find_nodes(g, pattern = "sickle trait"),
    to = find_nodes(g, pattern = "Malaria, Cerebral"),
    weights = w
)
paths_subset_reweight2 <- paths_subset_reweight2[[1]]
set.seed(64)
rand_idx <- sample.int(length(paths_subset_reweight2), 50)
paths_subset_reweight2 <- paths_subset_reweight2[rand_idx]
nodes2 <- do.call(c, paths_subset_reweight2) %>% unique()

## Readjust weights
w <- make_edge_weights(g, e_feat, node_semtypes_out = c("humn", "popg"), node_semtypes_in = c("gngm", "aapp"))
paths_subset_reweight3 <- find_paths(
    graph = g,
    from = find_nodes(g, pattern = "sickle trait"),
    to = find_nodes(g, pattern = "Malaria, Cerebral"),
    weights = w
)
paths_subset_reweight3 <- paths_subset_reweight3[[1]]
nodes3 <- do.call(c, paths_subset_reweight3) %>% unique()

nodes_sickle_trait <- nodes_sickle[c(4,9)]
nbrs_sickle_trait <- grow_nodes(g, nodes_sickle_trait)

all_nodes <- c(nodes, nodes1, nodes2, nodes3, nbrs_sickle_trait) %>% unique()

g_small <- induced_subgraph(g, all_nodes)
format(object.size(g_small), units = "Mb")

usethis::use_data(g_small, overwrite = TRUE)
