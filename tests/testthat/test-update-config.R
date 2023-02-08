describe("update_config_needs", {
  expect_silent({
    f <- file.path(testthat::test_path(), "fix", "pkg_convert", "DESCRIPTION")
    dcf <- read_dcf_asis(f)
  })

  # assure that fixture does not already contain entry
  expect_false("Config/Needs/documentation" %in% names(dcf))
  expect_false(any(grepl("roxytypes", dcf)))

  it("updates DESCRIPTION to add Config/Needs/.. section", {
    expect_silent(res <- update_config_needs(dcf))
    expect_true("Config/Needs/documentation" %in% names(res))
    expect_true(any(grepl("roxytypes", res)))
  })

  it("does not add additional Config/Needs/.. section if one exists", {
    expect_silent({
      res <- update_config_needs(dcf)
      res <- update_config_needs(res)
    })

    expect_true(sum("Config/Needs/documentation" == names(res)) == 1)
    expect_true({
      entries <- strsplit(res[["Config/Needs/documentation"]], "\n")[[1]]
      sum("roxytypes" == trimws(entries)) == 1
    })
  })
})


describe("update_config_roxygen_expr", {
  it("warns when provided expression is actually a try-error", {
    expect_warning(
      out <- update_config_roxygen_expr(try(1 + "a", silent = TRUE)),
      "parse.*Roxygen"
    )

    expect_null(out)
  })

  it("warns when expression is too complex to appropriately modify", {
    expr <- quote({
      res <- list(markdown = TRUE)
      packages <- c("roxytypes")
      res[["packages"]] <- packages
      res
    })

    expect_warning(out <- update_config_roxygen_expr(expr), "too complex")
    expect_null(out)

    expr <- quote(list(
      markdown = TRUE,
      packages = paste0("roxy", c("lint", "types"))
    ))

    expect_warning(out <- update_config_roxygen_expr(expr), "too complex")
    expect_null(out)
  })

  it("adds a $packages field if one does not already exist", {
    expect_silent(out <- update_config_roxygen_expr(quote(
      list(markdown = TRUE)
    )))

    expect_silent(out <- eval(out))
    expect_named(out, c("markdown", "packages"))
    expect_identical(out$packages, "roxytypes")
  })

  it("adds an entry to $packages field if it already exists", {
    expect_silent(out <- update_config_roxygen_expr(quote(
      list(markdown = TRUE, packages = "other")
    )))

    expect_silent(out <- eval(out))
    expect_named(out, c("markdown", "packages"))
    expect_identical(out$packages, c("other", "roxytypes"))

    expect_silent(out <- update_config_roxygen_expr(quote(
      list(markdown = TRUE, packages = c("other"))
    )))

    expect_silent(out <- eval(out))
    expect_named(out, c("markdown", "packages"))
    expect_identical(out$packages, c("other", "roxytypes"))
  })

  it("does not add a 'roxytypes' entry if it already exists in $packages", {
    expect_silent(out <- update_config_roxygen_expr(quote(
      list(markdown = TRUE, packages = c("other", "roxytypes"))
    )))

    expect_silent(out <- eval(out))
    expect_named(out, c("markdown", "packages"))
    expect_identical(out$packages, c("other", "roxytypes"))
  })
})


describe("update_config_roxygen_desc", {
  expect_silent({
    f <- file.path(testthat::test_path(), "fix", "pkg_convert", "DESCRIPTION")
    dcf <- read_dcf_asis(f)
  })

  # assure that fixture does not already contain entry
  expect_false("Roxygen" %in% names(dcf))

  it("adds a new Roxygen field when one does not exist", {
    expect_silent(res <- update_config_roxygen_desc(dcf))
    expect_true("Roxygen" %in% names(res))
  })

  it("adds a new Roxygen field with expected, parsable R structure", {
    expect_silent(res <- update_config_roxygen_desc(dcf))
    expect_silent(config <- eval(
      parse(text = res[["Roxygen"]]),
      envir = baseenv()
    ))

    expect_type(config, "list")
    expect_named(config, "packages")
    expect_identical(config$packages, "roxytypes")
    expect_length(config$packages, 1)
  })

  it("does not add additional Roxygen field when one exists", {
    expect_silent(res <- update_config_roxygen_desc(dcf))
    expect_silent(res <- update_config_roxygen_desc(res))
    expect_true(sum("Roxygen" == names(res)) == 1)
  })

  it("updates existing Roxygen field to add appropriate R structure", {
    expect_silent({
      res <- update_config_roxygen_desc(dcf)
      res[["Roxygen"]] <- "list(markdown = TRUE)"
      res <- update_config_roxygen_desc(res)
    })

    expect_silent(config <- eval(
      parse(text = res[["Roxygen"]]),
      envir = baseenv()
    ))

    expect_type(config, "list")
    expect_named(config, c("markdown", "packages"))
    expect_identical(config$packages, "roxytypes")
    expect_identical(config$markdown, TRUE)
    expect_length(config$packages, 1)
  })

  it("emits a warning when existing Roxygen section fails to parse", {
    expect_silent({
      res <- update_config_roxygen_desc(dcf)
      res[["Roxygen"]] <- "list(markdown"  # unterminated expression
    })

    expect_warning(
      res <- update_config_roxygen_desc(res),
      "parse.*Roxygen"
    )
  })
})


describe("update_config_roxygen_meta", {
  # set up some temporary testing fixtures
  expect_silent({
    tmp_file_no_roxytypes <- tempfile("meta_no_roxytypes.R")
    writeLines(con = tmp_file_no_roxytypes, c(
      "list(",
      "  markdown = TRUE",
      ")"
    ))

    tmp_file_roxytypes <- tempfile("meta_roxytypes.R")
    writeLines(con = tmp_file_roxytypes, c(
      "list(",
      "  markdown = TRUE,",
      "  packages = 'roxytypes'",
      ")"
    ))
  })

  it("updates file contents if roxytypes content is missing", {
    expect_silent(old <- eval(parse(text = readLines(tmp_file_no_roxytypes))))
    expect_type(old, "list")
    expect_named(old, "markdown")
    expect_silent(update_config_roxygen_meta(tmp_file_no_roxytypes))
    expect_silent(new <- eval(parse(text = readLines(tmp_file_no_roxytypes))))
    expect_type(new, "list")
    expect_named(new, c("markdown", "packages"))
    expect_identical(new$packages, "roxytypes")
  })

  it("updates file contents remain unchanged if already populated", {
    expect_silent(old <- eval(parse(text = readLines(tmp_file_roxytypes))))
    expect_silent(update_config_roxygen_meta(tmp_file_roxytypes))
    expect_silent(new <- eval(parse(text = readLines(tmp_file_roxytypes))))
    expect_identical(old, new)
  })
})


describe("make_config_edits", {
  pkg <- file.path(testthat::test_path(), "fix", "pkg_convert")

  it("modifies DESCRIPTION Roxygen if man/roxygen/meta.R does not exist", {
    # setup
    expect_silent({
      dir.create(to <- tempfile("pkg_convert"))
      pkg_tmp <- file.path(to, basename(pkg))
      file.copy(pkg, to, recursive = TRUE)
      unlink(file.path(pkg_tmp, "man"), recursive = TRUE)
    })

    expect_silent(modified <- make_config_edits(pkg_tmp))
    expect_identical(modified, "DESCRIPTION")
  })

  it("modifies man/roxygen/meta.R if exists", {
    # setup
    expect_silent({
      dir.create(to <- tempfile("pkg_convert"))
      pkg_tmp <- file.path(to, basename(pkg))
      file.copy(pkg, to, recursive = TRUE)
    })

    expect_silent(modified <- make_config_edits(pkg_tmp))
    expect_true(any(grepl("meta\\.R$", modified)))
  })
})
