test_that("@typed tag parsing accepts fields", {
  expect_silent(block <- roxygen2::parse_text("
    #' @typed var: type
    #'   this is a long form description
    f <- function(var) {
    }
  "))

  expect_silent(tag_typed <- block[[1]]$tags[[1]])
  expect_s3_class(tag_typed, "roxy_tag_typed")
  expect_identical(
    tag_typed$val,
    list(
      name = "var",
      type = "type",
      description = "this is a long form description"
    )
  )
})
