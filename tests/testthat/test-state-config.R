test_that("config can be loaded from a DESCRIPTION file", {
  on.exit(clear_state())

  expect_silent(clear_state())
  p <- file.path(testthat::test_path(), "fix", "pkg_config_desc")
  mockery::stub(config, "getwd", p)
  expect_identical(as.list(config()), list(verbose = FALSE))

  # and restored on next use
  mockery::stub(config, "getwd", function(i) stop())
  expect_identical(as.list(config()), list(verbose = FALSE))
})

test_that("config cache is cleared with global state", {
  on.exit(clear_state())

  expect_silent(clear_state())
  mockery::stub(config, "getwd", function(i) stop())
  expect_error(config())
})

test_that("config can be loaded from a man/roxytypes/meta.R file", {
  on.exit(clear_state())

  p <- file.path(testthat::test_path(), "fix", "pkg_config_meta")
  mockery::stub(config, "getwd", p)
  expect_identical(as.list(config()), list(format = "f", verbose = TRUE))
})
