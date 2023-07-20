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
      name = structure("var",
        class = c("roxy_tag_field_name", "roxy_tag_field", "character")
      ),
      type = structure("type",
        class = c("roxy_tag_field_type", "roxy_tag_field", "character")
      ),
      description = structure("this is a long form description",
        class = c("roxy_tag_field_description", "roxy_tag_field", "character")
      )
    )
  )
})

test_that("@typed tag parsing preserves special characters", {
  expect_silent(block <- roxygen2::parse_text("
    #' @typed var: \"`A` or `B`\"
    #'   this is a long form description
    f <- function(var) {
    }
  "))

  expect_silent(tag_typed <- block[[1]]$tags[[1]])
  expect_s3_class(tag_typed, "roxy_tag_typed")
  expect_identical(as.character(tag_typed$val$type), "`A` or `B`")

  expect_match(
    paste0(capture.output(roxygen2::roxy_tag_rd(tag_typed)), collapse = "\n"),
    "(`A` or `B`)",
    fixed = TRUE
  )
})

test_that("@typed tag custom formatting", {
  expect_silent(block <- roxygen2::parse_text("
    #' @typed var: type | type2
    #'   this is a long form description
    f <- function(var) {
    }
  "))

  # mock a custom config, providing a formatting function
  mockery::stub(roxy_tag_rd.roxy_tag_typed, "config", list(
    format = function(tag, ..., name, type, description) {
      types <- paste0("`", trimws(strsplit(type, "|", fixed = TRUE)[[1]]), "`")
      types <- glue::glue_collapse(types, sep = ", ", last = " or ")
      paste0("(", types, ") ", description)
    }
  ))

  expect_silent(tag_typed <- block[[1]]$tags[[1]])
  expect_silent(tag_rd <- roxy_tag_rd(tag_typed))
  expect_match(tag_rd$value, "(`type` or `type2`)", fixed = TRUE)
})
