library(testthat)
source("R/chart_config.R")
source("R/storage.R")

test_that("save/load chart config funktioniert", {
  tf <- tempfile(fileext = ".sqlite")
  con <- init_storage(tf)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cfg <- new_chart_config(chart_name = "T1")
  s <- save_chart_config(con, cfg)

  loaded <- load_chart_config(con, s$chart_id)
  expect_true(is.list(loaded))
  expect_equal(loaded$chart_meta$chart_name, "T1")
})
