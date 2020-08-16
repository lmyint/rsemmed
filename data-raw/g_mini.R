## code to prepare `g_mini` dataset goes here

library(tidyverse)
library(igraph)

load("~/GoogleDriveMac/research/public_data/SemMedDB/g.rda")
source("R/utils.R")
source("R/display.R")
source("R/find.R")
source("R/grow.R")
source("R/summarize.R")

nodes_cortisol <- find_nodes(g, "cortisol")
nodes_cortisol <- nodes_cortisol[c(2,18)]
nodes_stress <- find_nodes(g, "stress")
nodes_stress <- nodes_stress[c(2,3,32)]
nodes_mood <- find_nodes(g, "mood")
nodes_mood <- nodes_mood[c(2,5)]

g_mini <- induced_subgraph(g, c(nodes_cortisol, nodes_stress, nodes_mood))
plot_path(g, c(nodes_cortisol, nodes_stress, nodes_mood))


data.frame(
    subject = tail_of(g_mini, E(g_mini))$name,
    predicate = E(g_mini)$predicate,
    object = head_of(g_mini, E(g_mini))$name
)

g_mini <- delete_edges(g_mini, c(6:7,18))

data.frame(
    subject = tail_of(g_mini, E(g_mini))$name,
    predicate = E(g_mini)$predicate,
    object = head_of(g_mini, E(g_mini))$name
)

plot_path(g_mini, V(g_mini))

usethis::use_data(g_mini, overwrite = TRUE)
