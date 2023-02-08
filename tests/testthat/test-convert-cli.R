describe("diff_lines", {
  it("produces old and new line numbers from diff", {
    expect_silent(suppressWarnings(
      d <- cli::diff_chr(c("a", "b", "c"), c("d", "e", "c", "f"))
    ))

    expect_identical(
      diff_lines(d),
      cbind(
        old = c(1, 2, NA, NA, 3, NA),
        new = c(NA, NA, 1, 2, 3, 4)
      )
    )
  })
})

describe("format_diff_chr", {
  it("provides diff with line number gutter", {
    expect_silent(suppressWarnings(
      d <- cli::diff_chr(c("a", "b", "c"), c("d", "e", "c", "f"))
    ))

    expect_silent(fmt <- format_diff_chr(d, offset = 10))
    expect_true(all(startsWith(fmt, c(
      "10   ",
      "11   ",
      "   10",
      "   11",
      "12 12",
      "   13"
    ))))
  })
})

describe("preview_convert_edit(s)", {
  stub_str <- "#' @param a (`type`) this is the description"
  mockery::stub(preview_convert_edit, "scan", stub_str)

  f <- preview_convert_edit
  mockery::stub(f, "scan", stub_str)
  mockery::stub(preview_convert_edits, "preview_convert_edit", f)

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

  it("prints visual diff of tag edit", {
    expect_silent({
      out <- cli::cli_format_method(preview_convert_edit(edit))
    })

    expect_length(out, 4)
    expect_match(out[[1]], "\\btest\\.R\\b")
    expect_identical(out[-1], c(
      "10   -#' @param a (`type`) this is the description",
      "   10+#' @typed a: type",
      "   11+#'   this is the description"
    ))
  })

  it("prints visual diff of tag edits", {
    edits <- do.call(rbind, rep(list(edit), 4))
    expect_silent({
      out <- cli::cli_format_method(preview_convert_edits(edits, n = 2))
    })

    expect_length(out, 1 + 5 * 2)
    expect_match(out[[1]], "Examples")
    expect_true(all(grepl("\\btest\\.R\\b", out[2 + 0:1 * 5])))

    expect_silent({
      out <- cli::cli_format_method(preview_convert_edits(edits, n = 20))
    })

    expect_length(out, 1 + 5 * 4)
    expect_match(out[[1]], "Examples")
    expect_true(all(grepl("\\btest\\.R\\b", out[2 + 0:3 * 5])))
  })
})

describe("convert_continue_prompt", {
  it("returns TRUE when user continues", {
    mockery::stub(convert_continue_prompt, "readline", "y")
    expect_identical(suppressMessages(convert_continue_prompt()), TRUE)

    mockery::stub(convert_continue_prompt, "readline", "Y")
    expect_identical(suppressMessages(convert_continue_prompt()), TRUE)
  })

  it("returns FALSE when user aborts", {
    mockery::stub(convert_continue_prompt, "readline", "n")
    expect_identical(suppressMessages(convert_continue_prompt()), FALSE)

    mockery::stub(convert_continue_prompt, "readline", "q")
    expect_identical(suppressMessages(convert_continue_prompt()), FALSE)
  })

  it("returns a value when user enters a numeric, or nothing", {
    mockery::stub(convert_continue_prompt, "readline", "42")
    expect_equal(suppressMessages(convert_continue_prompt()), 42)

    mockery::stub(convert_continue_prompt, "readline", "")
    expect_equal(suppressMessages(convert_continue_prompt()), 3)
  })

  it("returns NULL when an unrecognizable value is entered", {
    mockery::stub(convert_continue_prompt, "readline", "unrecognized!")
    expect_identical(suppressMessages(convert_continue_prompt()), NULL)
  })
})
