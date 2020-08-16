context("Finding/filtering nodes")
library(rsemmed)

data(g_mini)

test_that("Regex for names works and ignores case", {
    nodes_cortisol <- find_nodes(g_mini, pattern = "CorTISOL")$name
    nodes_cortisol2 <- find_nodes(g_mini, pattern = "CORTIsoL$")$name
    expect_setequal(nodes_cortisol, c("hypercortisolemia", "Serum cortisol"))
    expect_setequal(sort(nodes_cortisol2), "Serum cortisol")
})

test_that("Checking for exact names works", {
    nodes1 <- find_nodes(g_mini, names = "Serum cort")
    nodes2 <- find_nodes(g_mini, names = "Serum cortisol")$name
    nodes3 <- find_nodes(g_mini, names = "serum COrtisol")$name

    expect_equal(length(nodes1), 0)
    expect_identical(nodes2, "Serum cortisol")
    expect_identical(nodes3, "Serum cortisol")
})

test_that("NULL for all arguments works", {
    nodes1 <- find_nodes(g_mini)
    nodes2 <- find_nodes(g_mini, match = FALSE)
    expect_equal(length(nodes1), 0)
    expect_equal(length(nodes2), 7)
})

test_that("match = FALSE works", {
    nodes <- find_nodes(g_mini, pattern = "stress") %>%
        find_nodes(pattern = "disorder", match = FALSE)
    nodes <- nodes$name %>% sort()
    expect_identical(nodes, c("Chronic Stress", "Stress"))
})

test_that("semtypes work alone and with pattern", {
    nodes1 <- find_nodes(g_mini, semtypes = "DSYN")$name
    target1 <- c("Stress", "Stress Disorders, Traumatic", "Mood Disorders", "hypercortisolemia")

    nodes2 <- find_nodes(g_mini, semtypes = c("dsyn", "FNDG"))$name
    target2 <- c(target1, "Chronic Stress")

    nodes3 <- find_nodes(g_mini, pattern = "cortisol", semtypes = "horm")$name
    target3 <- c("Serum cortisol", "hypercortisolemia")

    nodes4 <- find_nodes(g_mini, pattern = "mood", semtypes = "comd")$name
    target4 <- c("Stress", "Mood Disorders", "Mood")
    
    expect_identical(sort(nodes1), sort(target1))
    expect_identical(sort(nodes2), sort(target2))
    expect_identical(sort(nodes3), sort(target3))
    expect_identical(sort(nodes4), sort(target4))
})

test_that("Chaining works", {
    nodes <- find_nodes(g_mini, pattern = "mood") %>%
        find_nodes(semtypes = "dsyn")

    expect_identical(nodes$name, "Mood Disorders")
})
