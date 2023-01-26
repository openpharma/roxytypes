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

## Configuring Formatting

The style of documentation can be configured using `Config/roxytypes`:

`DESCRIPTION`
```
Config/roxytypes: list(format = "(`{type}`): {description}")
```

The format string uses `glue` and can be expected to have fields `name`, `type`
and `description`. The parameter name will always be the named argument value,
but may be reused for parts of the description.

## [`roxylint`](https://github.com/dgkf/roxylint) compatible

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

