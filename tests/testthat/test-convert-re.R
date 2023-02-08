describe("build_format_regex", {
  it("escapes any regex special characters from the glue expression", {
    expect_silent(out <- build_format_regex("({type})"))
    expect_true(grepl("\\^\\\\\\(", out))
    expect_true(grepl("\\\\\\)\\$$", out))
  })

  it("uses dotall syntax, allowing regex `.` to span multiple lines", {
    expect_silent(out <- build_format_regex("({type})"))
    expect_match(out, "^\\(\\?s\\)")
  })

  it("allows masking of type and description patterns", {
    expect_silent(out <- build_format_regex("({type})", type = "TYPE"))
    expect_match(out, "\\(\\?<type>TYPE\\)")

    expect_silent(out <- build_format_regex("({description})", description = "DESC"))  # nolint
    expect_match(out, "\\(\\?<description>DESC\\)")
  })

  it("allows passing regular expression using format_re directly", {
    expect_silent(out <- build_format_regex("({type})", format_re = ".*"))
    expect_identical(out, "(?s)^.*$")
  })

  it("uses regular description if no format is provided", {
    expect_silent(out <- build_format_regex())
    expect_match(out, "description")
  })
})
