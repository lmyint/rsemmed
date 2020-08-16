context("Displaying paths")
library(rsemmed)

data(g_mini)

test_that("text_path() works", {
    node1 <- find_nodes(g_mini, pattern = "Serum cortisol")
    node2 <- find_nodes(g_mini, pattern = "^mood$")
    node3 <- find_nodes(g_mini, pattern = "Stress Disorders, Traumatic")
    paths1 <- find_paths(g_mini, from = node1, to = node2)
    paths2 <- find_paths(g_mini, from = node1, to = node3)

    tp1 <- text_path(g_mini, paths1[[1]][[1]])
    tp2 <- text_path(g_mini, paths2[[1]][[1]])

    expect_equal(length(tp1), 1)
    expect_equal(length(tp2), 2)
    expect_equal(nrow(tp1[[1]]), 1)
    expect_equal(nrow(tp2[[1]]), 1)
    expect_equal(nrow(tp2[[2]]), 2)
})
