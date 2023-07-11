# `roxytypes`

<!-- badges: start -->
[![CRAN](https://img.shields.io/cran/v/roxytypes.svg)](https://cran.r-project.org/package=roxytypes)
[![R-CMD-check](https://github.com/openpharma/roxytypes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openpharma/roxytypes/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://img.shields.io/codecov/c/github/openpharma/roxytypes/main.svg)](https://app.codecov.io/gh/openpharma/roxytypes)
<!-- badges: end -->

Typed parameter definition `roxygen2` tag

## Quick Start

### Convert your package

If you already have a codebase and you'd like to convert it to use `roxytypes`,
you can use:

```r
roxytypes::convert()
```

You'll be prompted with a preview of changes and the option to continue by
making edits or aborting changes. By default, will look for type signatures as
parenthesized inline code at the start of your descriptions. That is, that
descriptions are written like `` (`type`) description ``.

If that's not the case, you can specify your own format. For example, if your
types were in square brackets, you could specify a format like:

```r
roxytypes::convert(format = "[`{type}`] {description}")
```

By default, conversions will only happen if an existing type is found using the
provided type format. If you'd like to convert all possible tags, pass
`unmatched = TRUE`. However new typed tags will only be partially populated and
will produce documentation notes until they are filled in.

### Tags from scratch 

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
#' @typedreturn: NULL
#'   `cat` output returned.
#'
hello <- function(who = "World") {
  cat("Hello, ", who, "!\n", sep = "")
}
```

Next order of business is to install the package and declare the 
`roxygen2` dependency. 

`DESCRIPTION`
```
Config/Needs/documentation:
    roxytypes
Roxygen:
    list(markdown = TRUE, packages = "roxytypes")
```

With all of that set up, the only thing left is to rebuild your docs!

## Configuration

`roxytypes` accepts a number of configuration fields. For defaults, see
`?config`.

### Formatting

The style of documentation can be configured using `Config/roxytypes`:

`DESCRIPTION`
```
Config/roxytypes: list(format = "(`{type}`): {description}")
```

The format string uses `glue` and can be expected to have fields `name`, `type`
and `description`. The parameter name will always be the named argument value,
but may be reused for parts of the description.

Altenatively, you can provide a function that accepts the parsed `roxygen2` tag
and the fields as named arguments.

For more advanced formatting, see `?tags` or `?typed`.

## [`roxylint`](https://github.com/openpharma/roxylint) compatible

`@typed` tags come with their own `roxylint` linters. To benefit from linting of
`@typed` tags, simply add the `roxylint::roxylint` roclet.

`DESCRIPTION`
```
Roxygen:
  list(
    markdown = TRUE,
    packages = c("roxylint", "roxytypes"),
    roclets = c("namespace", "rd", "roxylint::roxylint")
  )
```

> ***tip!***
>
> If your `Roxygen` section gets too long, you can also put this content in
> `man/roxygen/meta.R` where you can benefit from all the perks of your editor's
> R file handling.

