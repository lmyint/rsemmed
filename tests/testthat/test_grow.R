context("Growing a node set")
library(rsemmed)

data(g_mini)

test_that("grow() works", {
    node1 <- find_nodes(g_mini, pattern = "Stress Disorders, Traumatic")
    node2 <- find_nodes(g_mini, pattern = "Chronic Stress")
    node3 <- find_nodes(g_mini, pattern = "Mood Disorders")

    nbrs1 <- grow_nodes(g_mini, node1)$name
    nbrs2 <- grow_nodes(g_mini, node2)$name %>% sort()
    nbrs3 <- grow_nodes(g_mini, c(node1, node2))$name %>% sort()
    nbrs4 <- grow_nodes(g_mini, c(node1, node2, node3))$name %>% sort()

    expect_identical(nbrs1, "Stress")
    expect_identical(nbrs2, c("Mood Disorders", "Stress"))
    expect_identical(nbrs3, c("Mood Disorders", "Stress"))
    expect_identical(nbrs4, 
        sort(c("Stress", "hypercortisolemia", "Serum cortisol", "Mood"))
    )
})
