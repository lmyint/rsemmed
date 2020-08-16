context("Utility functions")
library(rsemmed)

data(g_mini)
e_feat <- get_edge_features(g_mini)

test_that("Dimensions of edge features reflect options", {
    e_feat1 <- get_edge_features(g_mini)
    e_feat2 <- get_edge_features(g_mini, include_degree = TRUE)
    e_feat3 <- get_edge_features(g_mini, include_node_ids = TRUE)
    e_feat4 <- get_edge_features(g_mini, include_num_instances = TRUE)

    expect_identical(dim(e_feat1), c(15L, 5L))
    expect_identical(dim(e_feat2), c(15L, 9L))
    expect_identical(dim(e_feat3), c(15L, 7L))
    expect_identical(dim(e_feat4), c(15L, 6L))
})

test_that("get_middle_nodes has correct output", {
    node1 <- find_nodes(g_mini, pattern = "Serum cortisol")
    node2 <- find_nodes(g_mini, pattern = "Chronic Stress")
    node3 <- find_nodes(g_mini, name = "mood")
    paths1 <- find_paths(g_mini, from = node1, to = node2)
    middle1 <- get_middle_nodes(g_mini, paths1)
    paths2 <- find_paths(g_mini, from = node1, to = node3)
    middle2 <- get_middle_nodes(g_mini, paths2)
    middle3 <- get_middle_nodes(g_mini, paths1, collapse = FALSE)

    expect_equal(length(middle1), 2)
    expect_equal(length(middle2), 0)
    expect_equal(nrow(middle3), 1)
    expect_equal(length(middle3$middle_nodes[[1]]), 2)
})

test_that("make_edge_weights works correctly for _out", {
    w1 <- make_edge_weights(g_mini, e_feat)
    w2 <- make_edge_weights(g_mini, e_feat, node_names_out = "chocolate")
    w3 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_out = "dsyn",
        node_names_out = "chocolate")
    w4 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_out = "dsyn",
        node_names_out = "mood") # capitalization shouldn't matter
    w5 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_out = "dsyn",
        node_names_out = "Mood")
    w6 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_out = c("dsyn", "horm", "fndg"))
    w7 <- make_edge_weights(g_mini, e_feat,
        node_names_out = c("Stress Disorders, Traumatic", "Mood")
    )
    w8 <- make_edge_weights(g_mini, e_feat,
        edge_preds_out = c("PREDISPOSES", "ISA")
    )

    expect_identical(w1, rep(1, gsize(g_mini)))
    expect_identical(w1, w2)
    expect_equal(sum(w3==7), 14)
    expect_equal(sum(w4==7), 15)
    expect_equal(sum(w5==7), 15)
    expect_true(all(w6==7))
    expect_identical(which(w7==7), as.integer(c(1,4,7,10)))
    expect_identical(which(w8==7), as.integer(c(1:3, 11:13)))
})

test_that("make_edge_weights works correctly for _in", {
    w2 <- make_edge_weights(g_mini, e_feat, node_names_in = "chocolate")
    w3 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_in = "dsyn",
        node_names_in = "chocolate")
    w4 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_in = "dsyn",
        node_names_in = "mood") # capitalization shouldn't matter
    w5 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_in = "dsyn",
        node_names_in = "Mood")
    w6 <- make_edge_weights(g_mini, e_feat, 
        node_semtypes_in = c("dsyn", "horm", "fndg"))
    w7 <- make_edge_weights(g_mini, e_feat,
        node_names_in = c("Stress Disorders, Traumatic", "Mood")
    )
    w8 <- make_edge_weights(g_mini, e_feat,
        edge_preds_in = c("PREDISPOSES", "ISA")
    )

    low_thresh <- 1e-9+(1/14)
    expect_identical(w2, rep(1, gsize(g_mini)))
    expect_equal(sum(w3 < low_thresh), 14)
    expect_equal(sum(w4 < low_thresh), 15)
    expect_equal(sum(w5 < low_thresh), 15)
    expect_true(all(w6 < low_thresh))
    expect_identical(which(w7 < low_thresh), as.integer(c(1,4,7,10)))
    expect_identical(which(w8 < low_thresh), as.integer(c(1:3, 11:13)))
})

test_that("make_edge_weights works correctly for _out and _in", {
    w1 <- make_edge_weights(g_mini, e_feat,
        edge_preds_in = "ISA", node_names_out = "Stress")
    expect_identical(
        which(w1 > 6.99), as.integer(c(1,2,3,4,5,11,12,14,15))
    )
    expect_equal(sum(w1 < 1), 6)
})
