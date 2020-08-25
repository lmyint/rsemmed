library(tidyverse)
library(igraph)

## Read in data
pred <- read_csv("../data/PREDICATION.csv.gz", col_types = "iiccccclcccl")

## Store semtype information
semtypes <- unique(c(pred$SUBJECT_SEMTYPE, pred$OBJECT_SEMTYPE))
semtypes <- sort(semtypes)
semtype_info <- lapply(semtypes, function(semt) {
    names_subj <- pred %>%
        dplyr::filter(SUBJECT_SEMTYPE==semt) %>%
        pull(SUBJECT_NAME)
    names_obj <- pred %>%
        dplyr::filter(OBJECT_SEMTYPE==semt) %>%
        pull(OBJECT_NAME)
    unique(c(names_subj, names_obj))
})
names(semtype_info) <- semtypes
write_rds(semtype_info, path = "../data/semtype_info.rds")

## Create igraph from SemMed ##
## First make edge list
el_df <- pred %>%
    dplyr::filter(SUBJECT_NOVELTY, OBJECT_NOVELTY) %>%
    select(SUBJECT_NAME, SUBJECT_SEMTYPE, PREDICATE, OBJECT_NAME, OBJECT_SEMTYPE) %>%
    group_by(SUBJECT_NAME, PREDICATE, OBJECT_NAME) %>%
    summarize(
        num_instances = n(),
        subj_semtype = paste(sort(SUBJECT_SEMTYPE), collapse = ","),
        obj_semtype = paste(sort(OBJECT_SEMTYPE), collapse = ",")
    )
el <- cbind(el_df$SUBJECT_NAME, el_df$OBJECT_NAME)
## Some names have multiple semtypes
semtype_df <- tibble(
    name = c(pred$SUBJECT_NAME, pred$OBJECT_NAME),
    semtype = c(pred$SUBJECT_SEMTYPE, pred$OBJECT_SEMTYPE)
)
semtype_df <- unique(semtype_df) %>%
    group_by(name) %>%
    summarize(semtype = paste(sort(semtype), collapse = ","))
## Next make the graph
g <- graph_from_edgelist(el, directed = TRUE)
## Edge attributes
edge_attr(g, "predicate") <- el_df$PREDICATE
edge_attr(g, "num_instances") <- el_df$num_instances
## Vertex attributes
v_names <- tibble(name = vertex_attr(g, "name"))
num_nodes <- nrow(v_names)
v_names <- v_names %>%
    left_join(semtype_df)
stopifnot(nrow(v_names)==num_nodes)
vertex_attr(g, "semtype") <- v_names$semtype

save(g, file = "../data/g.rda")
