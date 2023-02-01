#' `roxytypes` tags
#'
#' The `@typed` tag introduces a syntax for defining parameter types as a
#' `roxygen2` tag.
#'
#' Be aware that there are a few syntactic requirements:
#'
#'   * `:` delimiter between the variable name and type.
#'   * `\n` after the type to separate it from the description.
#'   * `@default` tag can be used to optionally specify a default parameter
#'     value. If not set, and `derive_defaults` is enabled, then atomic,
#'     length-1 parameter defaults will automatically be used to populate
#'     default arguments in documentation.
#'
#' @usage
#' #' @typed <var>: <type>
#' #'   <description>
#'
#' #' @typed <var>: <type> @default <default>
#' #'   <description>
#'
#' @aliases default typed
#' @name tags
NULL


#' `roxygen2` `@typed` tag parsing
#'
#' Parse a `@typed` tag and return parsed components as value
#'
#' @inheritParams roxygen2::roxy_tag_parse
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @exportS3Method
roxy_tag_parse.roxy_tag_typed <- function(x) {  # nolint
  parsed <- try_parse_typed(x$raw)

  if (is.null(parsed)) {
    er <- "<parse error>"
    roxygen2::warn_roxy_tag(x, errors$parse_syntax(x$tag))
    x$val <- list(name = er, type = er, description = er)
    return(x)
  }

  x$val <- as.list(trimws(parsed[1, ]))
  x
}


#' `roxygen2` `@typed` tag rd section population
#'
#' Push typed fields into `@param` section
#'
#' @inheritParams roxygen2::roxy_tag_rd
#'
#' @importFrom glue glue
#' @importFrom roxygen2 roxy_tag_rd
#' @exportS3Method
roxy_tag_rd.roxy_tag_typed <- function(x, base_path, env) {  # nolint
  config <- config()

  # fetch associated parameter default, or use existing definition
  x$val$default <- get_parameter_default(x)

  # format typed tag
  format <- config$format %||% default_format
  desc <- if (is.function(format)) {
    do.call(format, append(list(x), x$val))
  } else {
    glue::glue(format, .envir = x$val)
  }

  names(desc) <- x$val$name
  roxygen2::rd_section("param", desc)
}


#' Get Associated Parameter Default
#'
#' Provided a [roxygen2::roxy_tag()], try to discover the associated default
#' parameter value.
#'
#' Default values can be discovered if they are length-1 atomic values.
#'
#' @typed x: [roxygen2::roxy_tag()]
#'   A tag to use to discover the associated function block and possible
#'   parameter defaults.
#'
#' @typedreturn character[1] | NULL
#'   Formatted representation of the value if found, falling back to a missing
#'   representation or `NULL` if a default value could not be discovered.
#'
#' @keywords internal
get_parameter_default <- function(x) {
  # return early if value is already set
  if (nchar(x$val$default) > 0)
    return(x$val$default)

  config <- config()
  missing <- config$defaults$missing
  warn <- isTRUE(config$defaults$warn_undocumented)

  block <- associated_block(x$file, x$line)
  fn <- block$object$value

  if (!is.function(fn))
    return(x$val$default)

  has_default <- !identical(formals(fn)[[x$val$name]], bquote())

  # not configured to derive defaults, warn that one wasn't found
  if (!isTRUE(config$defaults$derive)) {
    if (has_default && warn)
      roxygen2::warn_roxy_tag(x, errors$default_undocumented(x$tag))

    return(x$val$default)
  }

  if (!has_default)
    return(missing)

  default <- formals(fn)[[x$val$name]]
  if (is.atomic(default) && (is.null(default) || length(default) == 1))
    return(deparse(default))

  # warn if parameters could be derived, but remains unset
  if (warn)
    roxygen2::warn_roxy_tag(x, errors$default_undocumented(x$tag))

  invisible("")
}


#' Default formatter for `@typed`
#'
#' Adds special cases for when the type uses other roxygen2 syntax
#'
#' @typed x: [roxygen2::roxy_tag()]
#'   The tag to format.
#' @typed name,type,default,description: character(1)
#'   Fields parsed from the `@typed` tag.
#' @param ... Additional arguments unused.
#'
#' @return A formatted character value.
#'
#' @keywords internal
default_format <- function(x, name, type, default = NULL, description, ...) {
  typestr <- type

  # do not wrap roxygen links in code backticks
  if (!grepl("^\\[.*\\]$", type))
    typestr <- paste0("`", typestr, "`")

  defaultstr <- ""
  if (is.character(default) && length(default) > 0 && nchar(default) > 0)
    defaultstr <- paste0("; Default = `", default, "`")

  paste0("(", typestr, defaultstr, ") ", description)
}


#' Parse a raw `@typed` tag
#'
#' @typed raw: character[1]
#'   A raw string from which to try to parse typed parameter definitions.
#'
#' @keywords internal
try_parse_typed <- function(raw) {
  re <- paste0(
    "(?<name>[^\\n:]*):",
    "(?<type>[^\\n]*?)",
    "(?: @default (?<default>[^\\n]*))?",
    "\\n(?<description>(?:.|\\n)*)"
  )

  res <- tryCatch(
    regex_capture(re, raw, perl = TRUE),
    error = function(e) NULL
  )

  if (any(nchar(res[, c("name", "description")]) == 0)) {
    return(NULL)
  }

  res
}
