context("Summarizing nodes and paths")
library(rsemmed)

data(g_mini)

test_that("summarize_semtypes() works", {
    node1 <- find_nodes(g_mini, pattern = "serum cortisol")
    node2 <- find_nodes(g_mini, names = "chronic stress")
    paths <- find_paths(g_mini, from = node1, to = node2)
    path <- paths[[1]][[1]]

    ss1 <- summarize_semtypes(g_mini, paths, print = FALSE)
    ss2 <- summarize_semtypes(g_mini, path, print = FALSE, is_path = FALSE)
    
    expect_equal(nrow(ss1), 1)
    expect_equal(nrow(ss2), 6)
})

test_that("summarize_predicates() works", {
    node1 <- find_nodes(g_mini, pattern = "serum cortisol")
    node2 <- find_nodes(g_mini, names = "chronic stress")
    paths <- find_paths(g_mini, from = node1, to = node2)
    
    sp <- summarize_predicates(g_mini, paths)
    
    expect_equal(nrow(sp), 1)
    expect_equal(length(sp$predicates[[1]]), 4)
    expect_equal(sp$predicates[[1]][["PREDISPOSES"]], 3)
    expect_equal(sp$predicates[[1]][["TREATS"]], 1)
    expect_equal(sp$predicates[[1]][["DISRUPTS"]], 1)
    expect_equal(sp$predicates[[1]][["ISA"]], 1)
})
