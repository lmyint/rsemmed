context("Finding paths")
library(rsemmed)

data(g_mini)

test_that("# paths, path lengths, and classes are correct", {
    node1 <- find_nodes(g_mini, pattern = "serum cortisol")
    node2 <- find_nodes(g_mini, pattern = "chronic stress")
    paths <- find_paths(g_mini, from = node1, to = node2)

    expect_true(is(paths, "list"))
    expect_true(is(paths[[1]], "list"))
    expect_true(is(paths[[1]][[1]], "igraph.vs"))
    expect_equal(length(paths), 1)
    expect_equal(length(paths[[1]]), 2)
    expect_equal(length(paths[[1]][[1]]), 3)
    expect_equal(length(paths[[1]][[2]]), 3)
})

test_that("lengths correct for multiple nodes", {
    nodes1 <- find_nodes(g_mini, pattern = "cortisol")
    nodes2 <- find_nodes(g_mini, pattern = "stress")
    paths <- find_paths(g_mini, from = nodes1, to = nodes2)
    paths_reverse <- find_paths(g_mini, from = nodes2, to = nodes1)

    expect_equal(length(paths), 2)
    expect_equal(length(paths_reverse), 3)
})

test_that("Weights work", {
    node1 <- find_nodes(g_mini, pattern = "serum cortisol")
    node2 <- find_nodes(g_mini, pattern = "chronic stress")
    
    w <- rep(1, gsize(g_mini))
    w[c(5,6,7)] <- 2*vcount(g_mini)

    paths <- find_paths(g_mini, from = node1, to = node2, weights = w)
    bool_md <- c(
        "Mood Disorders" %in% paths[[1]][[1]]$name,
        "Mood Disorders" %in% paths[[1]][[2]]$name
    )
    path_thru_md <- paths[[1]][bool_md]
    path_thru_md <- path_thru_md[[1]]$name

    other_path <- paths[[1]][!bool_md]
    other_path <- other_path[[1]]$name

    expect_equal(length(paths[[1]]), 2)
    expect_identical(path_thru_md, c("Serum cortisol", "hypercortisolemia", "Mood Disorders", "Chronic Stress"))
    expect_identical(other_path, c("Serum cortisol", "hypercortisolemia", "Stress", "Chronic Stress"))
})
