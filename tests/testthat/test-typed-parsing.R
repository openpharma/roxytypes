test_that("@typed parsing internals extract fields", {
  expect_silent(res <- try_parse_typed("var: type\ndescription"))
  expect_identical(
    res[1, ],
    c(name = "var", type = " type", default = "", description = "description")
  )

  expect_silent(res <- try_parse_typed("var: space type\nspace description"))
  expect_identical(
    res[1, ],
    c(
      name = "var",
      type = " space type",
      default = "",
      description = "space description"
    )
  )

  expect_silent(res <- try_parse_typed(
    "var: space type @default space default\nspace description"
  ))
  expect_identical(
    res[1, ],
    c(
      name = "var",
      type = " space type",
      default = "space default",
      description = "space description"
    )
  )
})
