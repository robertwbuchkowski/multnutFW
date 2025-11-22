
test_that("adds node column and binds correctly", {
  df_a <- data.frame(id = 1:2, value = c(10, 20))
  df_b <- data.frame(id = 3:4, value = c(30, 40))
  dfs <- list(A = df_a, B = df_b)

  res <- add_node_column(dfs)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  expect_true("Node" %in% names(res))
  expect_equal(res$Node, c("A", "A", "B", "B"))
})

test_that("custom node_col works", {
  df_a <- data.frame(id = 1, value = 10)
  df_b <- data.frame(id = 2, value = 20)
  dfs <- list(A = df_a, B = df_b)

  res <- add_node_column(dfs, node_col = "group")
  expect_true("group" %in% names(res))
  expect_equal(res$group, c("A", "B"))
})

test_that("errors on unnamed list", {
  df_a <- data.frame(id = 1)
  df_b <- data.frame(id = 2)
  dfs <- list(df_a, df_b)
  expect_error(add_node_column(dfs), "named")
})

test_that("errors when any element is not a data frame", {
  dfs <- list(A = data.frame(id = 1), B = 2)
  expect_error(add_node_column(dfs), "data frames")
})
