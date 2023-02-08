test_that("roxygen_blocks can be scraped for topenv namespace", {
  expect_silent(clear_state())

  tp <- normalizePath(testthat::test_path())
  p <- file.path(tp, "fix", "pkg_config_desc")

  expect_true({
    blocks <- roxygen_blocks(path = p)
    TRUE
  })

  expect_true(length(blocks) > 0)
})

test_that("associated_block can find the block containing a tag", {
  tp <- normalizePath(testthat::test_path())
  f <- normalizePath(file.path(tp, "fix", "pkg_config_desc", "R", "fn.R"))
  expect_s3_class(associated_block(f, 3), "roxy_block")
  expect_null(associated_block(f, 1e5))
})
