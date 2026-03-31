library(testthat)
source("R/transformation_engine.R")

test_that("topo sort arbeitet linear", {
  tr <- list(
    list(id = "tr_a", type = "identity", inputs = c("raw_1"), params = list(), output_label = "A"),
    list(id = "tr_b", type = "sma", inputs = c("tr_a"), params = list(window = 3L), output_label = "B")
  )

  ord <- topo_sort_transformations(tr)
  expect_equal(ord, c("tr_a", "tr_b"))
})

test_that("zyklus wird erkannt", {
  tr <- list(
    list(id = "tr_a", type = "identity", inputs = c("tr_b"), params = list(), output_label = "A"),
    list(id = "tr_b", type = "identity", inputs = c("tr_a"), params = list(), output_label = "B")
  )

  cyc <- check_circular_dependencies(tr)
  expect_false(cyc$ok)
})
