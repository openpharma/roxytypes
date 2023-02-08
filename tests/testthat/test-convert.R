describe("tag_edit", {
  it("produces a edit record given a tag", {
    expect_silent({
      tag <- roxygen2::roxy_tag(
        tag = "param",
        raw = "a (`type`) this is the description",
        val = list(name = "a", description = "this is the description"),
        file = "./R/test.R",
        line = 10
      )

      edit <- tag_edit(
        tag,
        c("#' @typed a: type", "#'   this is the description"),
        matched = TRUE
      )
    })

    expect_named(edit, c("file", "line", "n", "matched", "new"))
    expect_equal(nrow(edit), 1)
    expect_type(edit$new, "list")
    expect_type(edit$matched, "logical")
  })
})


describe("convert_match_format", {
  it("provides structured capture groups", {
    expect_silent(res <- convert_match_format(
      "type description",
      "(?<type>type) (?<description>.*)"
    ))

    expect_type(res, "list")
    expect_named(res, c("type", "description", "matched"))
    expect_equal(unname(res$type), "type")
    expect_equal(unname(res$description), "description")
    expect_true(res$matched)
  })

  it("falls back to providing all content as description", {
    expect_silent(res <- convert_match_format(
      "type description",
      "(?<type>type)--(?<description>.*)"
    ))

    expect_type(res, "list")
    expect_named(res, c("type", "description", "matched"))
    expect_equal(unname(res$type), "")
    expect_equal(unname(res$description), "type description")
    expect_false(res$matched)
  })

  it("works when capture group names are not provided", {
    expect_silent(res <- convert_match_format(
      "type description",
      ".*"
    ))

    expect_type(res, "list")
    expect_named(res, c("type", "description", "matched"))
    expect_equal(unname(res$type), "")
    expect_equal(unname(res$description), "type description")
    expect_false(res$matched)
  })
})


describe("convert_tag", {
  # type only matches TYPE, remainder is the description
  format <- "(?s)^(?<type>TYPE) (?<description>.*)$"

  it("converts @return roxygen tags", {
    expect_silent({
      tag <- roxygen2::roxy_tag(tag = "return", raw = "TYPE a value",
        val = "TYPE a value", file = "./R/test.R", line = 10)
    })

    expect_silent(edit <- convert_tag(tag, format))
    expect_identical(edit$new[[1]], c(
      "#' @typedreturn TYPE",
      "#'   a value"
    ))
  })

  it("converts @param roxygen tags", {
    expect_silent({
      tag <- roxygen2::roxy_tag(tag = "param", raw = "var TYPE a value",
        val = list(name = "var", description = "TYPE a value"),
        file = "./R/test.R", line = 10)
    })

    expect_silent(edit <- convert_tag(tag, format))
    expect_identical(edit$new[[1]], c(
      "#' @typed var: TYPE",
      "#'   a value"
    ))
  })

  it("returns NULL when it can't handle a tag", {
    expect_silent({
      tag <- roxygen2::roxy_tag(tag = "details", raw = "details",
        val = "details", file = "./R/test.R", line = 10)
    })

    expect_silent(edit <- convert_tag(tag, format))
    expect_identical(edit, NULL)
  })
})


describe("build_convert_edits", {
  # type only matches TYPE, remainder is the description
  format <- "(?s)^(?<type>TYPE) (?<description>.*)$"

  tag_ret <- roxygen2::roxy_tag(tag = "return", raw = "TYPE a value",
    val = "TYPE a value", file = "./R/test.R", line = 10)

  tag_param_match <- roxygen2::roxy_tag(tag = "param", raw = "var TYPE a value",
    val = list(name = "var", description = "TYPE a value"),
    file = "./R/test.R", line = 20)

  tag_param_no_match <- roxygen2::roxy_tag(tag = "param", raw = "var T a value",
    val = list(name = "var", description = "T a value"),
    file = "./R/test.R", line = 30)

  tag_det <- roxygen2::roxy_tag(tag = "details", raw = "details",
    val = "details", file = "./R/test.R", line = 40)

  tags <- list(tag_ret, tag_param_match, tag_param_no_match, tag_det)

  it("prepares a listing of possible edits", {
    expect_silent(edits <- build_convert_edits(format, tags))
    expect_equal(nrow(edits), 2)
    expect_equal(edits$line, c(10, 20))
  })

  it("includes unmatched valid tags when unmatched = TRUE", {
    expect_silent(edits <- build_convert_edits(format, tags, unmatched = TRUE))
    expect_equal(nrow(edits), 3)
    expect_equal(edits$line, c(10, 20, 30))
  })
})


describe("make_convert_edits", {
  # setup, clone convert fixture package to temp
  pkg <- file.path(testthat::test_path(), "fix", "pkg_convert")
  pkg_tmp <- file.path(f <- tempfile(), basename(pkg))
  dir.create(f)
  file.copy(pkg, f, recursive = TRUE)

  # save originals for comparison
  a_old <- readLines(file.path(pkg_tmp, "R", "a.R"))
  b_old <- readLines(file.path(pkg_tmp, "R", "b.R"))

  # ensure we can build our tags from the fixture package
  expect_silent(suppressMessages({
    blocks <- roxygen_blocks(path = pkg_tmp, refresh = TRUE, cache = FALSE)
    tags <- unlist(lapply(blocks, `[[`, "tags"), recursive = FALSE)
  }))

  # and that we can form some edits
  expect_silent({
    format <- build_format_regex("(`{type}`) {description}")
    edits <- build_convert_edits(format, tags)
  })

  expect_equal(nrow(edits), 3)

  it("performs provided edits", {
    expect_silent(make_convert_edits(edits))

    a_new <- readLines(file.path(pkg_tmp, "R", "a.R"))
    b_new <- readLines(file.path(pkg_tmp, "R", "b.R"))

    expect_true(!identical(a_old, a_new))
    expect_true(!identical(b_old, b_new))

    expect_true(!any(grepl("@typed\\b", a_old)))
    expect_true(any(grepl("@typed\\b", a_new)))

    expect_true(!any(grepl("@typedreturn", a_old)))
    expect_true(any(grepl("@typedreturn", a_new)))

    expect_true(!any(grepl("@typed\\b", b_old)))
    expect_true(any(grepl("@typed\\b", b_new)))
  })
})


describe("convert", {
  # setup, clone convert fixture package to temp
  pkg <- file.path(testthat::test_path(), "fix", "pkg_convert")
  pkg_tmp <- file.path(f <- tempfile(), basename(pkg))
  dir.create(f)
  file.copy(pkg, f, recursive = TRUE)

  # save originals for comparison
  roxy_meta_old <- readLines(file.path(pkg_tmp, "man", "roxygen", "meta.R"))
  desc_old <- readLines(file.path(pkg_tmp, "DESCRIPTION"))
  a_old <- readLines(file.path(pkg_tmp, "R", "a.R"))
  b_old <- readLines(file.path(pkg_tmp, "R", "b.R"))

  # automatically agree to convert, akin to entering "y" at prompt
  f <- convert_continue_prompt
  mockery::stub(f, "readline", "y")
  mockery::stub(convert, "convert_continue_prompt", f)

  it("converts package tags", {
    expect_silent(expect_true(suppressMessages({
      convert("(`{type}`) {description}", path = pkg_tmp, verbose = TRUE)
    })))

    roxy_meta_new <- readLines(file.path(pkg_tmp, "man", "roxygen", "meta.R"))
    desc_new <- readLines(file.path(pkg_tmp, "DESCRIPTION"))
    a_new <- readLines(file.path(pkg_tmp, "R", "a.R"))
    b_new <- readLines(file.path(pkg_tmp, "R", "b.R"))

    expect_true(!identical(roxy_meta_old, roxy_meta_new))
    expect_true(!identical(a_old, a_new))
    expect_true(!identical(b_old, b_new))
    expect_true(!identical(desc_old, desc_new))

    expect_true(!any(grepl("roxytypes", roxy_meta_old)))
    expect_true(any(grepl("roxytypes", roxy_meta_new)))

    expect_true(!any(grepl("Config/Needs", desc_old)))
    expect_true(any(grepl("Config/Needs", desc_new)))

    expect_true(!any(grepl("Roxygen", desc_old)))
    expect_true(!any(grepl("Roxygen", desc_new)))
  })
})
