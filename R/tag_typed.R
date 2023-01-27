#' `roxytypes` tags
#'
#' The `@typed` tag introduces a syntax for defining parameter types as a
#' `roxygen2` tag.
#'
#' There are a couple important syntactic features:
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
  config <- config_load()

  # if feature enabled and default not manually set, derive from definition
  if (isTRUE(config$defaults$derive) && nchar(x$val$default) == 0) {
    x$val$default <- get_param_default(x, missing = config$defaults$missing)
  }

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
#' @typed missing: character[1]
#'   If non-`NULL`, a value to use when a parameter is by default undefined.
#'
#' @typedreturn character[1] | NULL
#'   Formatted representation of the value if found, falling back to a missing
#'   representation or `NULL` if a default value could not be discovered.
#'
#' @keywords internal
get_param_default <- function(x, missing = NULL) {
  block <- associated_block(x$file, x$line)

  if (is.function(fn <- block$object$value)) {
    fn_formals <- formals(fn)
    d <- fn_formals[[x$val$name]]
    if (identical(fn_formals[[x$val$name]], bquote())) {
      missing
    } else if (is.atomic(d) && (is.null(d) || length(d) == 1)) {
      deparse(d)
    }
  }
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
