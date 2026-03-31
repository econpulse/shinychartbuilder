library(testthat)
source("R/chart_config.R")
source("R/validators.R")

test_that("validate_axes_config erkennt ungueltige Limits", {
  x <- list(date_breaks = "1 month", date_labels = "%Y-%m", limits = c("2024-12-31", "2024-01-01"))
  y <- list(min = 10, max = 1, breaks = NULL, format = "number", decimals = 2)

  res <- validate_axes_config(x, y)
  expect_false(res$ok)
  expect_true(length(res$errors) >= 1)
})

test_that("validate_chart_config ist fuer Default-Konfiguration ok", {
  cfg <- new_chart_config()
  res <- validate_chart_config(cfg)
  expect_true(res$ok)
})
