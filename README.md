# `roxytypes`

<!-- badges: start -->
[![R-CMD-check](https://github.com/dgkf/roxytypes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dgkf/roxytypes/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Typed parameter definition `roxygen2` tag

## Quick Start

Use the `@typed` tag to define parameters, replacing the `@param` tag.

The `@typed` tag expects input in the form:

```
#' @typed <var>: <type>
#'   <description>
```

The newline after the `type` field is a meaningful delimiter to avoid having to
disambiguate between type annotations and written descriptions. In practice it
looks something like this:

```r
#' Example
#'
#' @typed who: character
#'   Who you'd like to say hello to.
#'
hello <- function(who = "World") {
  cat("Hello, ", who, "!\n", sep = "")
}
```

Install the `roxytypes` package. 

`DESCRIPTION`
```
Config/Needs/documentation:
    roxytypes
Roxygen:
    list(markdown = TRUE, packages = "roxytypes")
```

Rebuild your docs!

## Configuring Formatting

The style of documentation can be configured using `Config/roxytypes`:

`DESCRIPTION`
```
Config/roxytypes: list(format = "(`{type}`): {description}")
```

The format string uses `glue` and can be expected to have fields `name`, `type`
and `description`. The parameter name will always be the named argument value,
but may be reused for parts of the description.

## Applying Style Checks

If you'd like to enforce a style guide for your parameters, you can set checks
as part of the config. This can be included in `Config/roxytypes`, but checking
code will probably be cumbersome to author in this format. Instead, you can use
`man/roxytypes/meta.R` which accepts the same format, but allows for more
comfortable development.

`man/roxytypes/meta.R`
```r
list(
  format = "(`{type}`): {description}",
  checks = function(name, type, description) {
    if (!endsWith(description, "."))
      warning("Parameter descriptions must end with a period")

    if (!grepl("^[[:upper:]`]", description))
      warning("Parameter descriptions should be 'Sentence case'")
  }
)
```

Warnings emitted in the `checks` function will be raised through `roxygen2` when
documentation is re-built and will include source information to help find
offending tags.
