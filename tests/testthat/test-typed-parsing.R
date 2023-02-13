test_that("@typed parsing internals extract fields", {
  expect_silent(res <- try_parse_typed("var: type\ndescription"))
  expect_identical(
    res[1, ],
    c(name = "var", type = " type", description = "description")
  )

  expect_silent(res <- try_parse_typed("var: space type\nspace description"))
  expect_identical(
    res[1, ],
    c(
      name = "var",
      type = " space type",
      description = "space description"
    )
  )

  expect_silent(res <- try_parse_typed(
    "var: space type\nspace description"
  ))
  expect_identical(
    res[1, ],
    c(
      name = "var",
      type = " space type",
      description = "space description"
    )
  )
})
