#' `roxytypes` tags
#'
#' The `@typed` tag introduces a syntax for defining parameter types as a
#' `roxygen2` tag.
#'
#' Be aware that there are a few syntactic requirements:
#'
#'  * `:` delimiter between the variable name and type.
#'  * `\n` after the type to separate it from the description.
#'
#' @usage
#' #' @typed <var>: <type>
#' #'   <description>
#'
#' @aliases typed
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

  # format typed tag
  format <- config$format %||% default_format
  desc <- if (is.function(format)) {
    do.call(format, append(list(x), x$val))
  } else {
    glue::glue(format, .envir = x$val)
  }

  # handle markdown-style formatting using roxygen2 internals
  if (isTRUE(roxygen2::roxy_meta_get("markdown"))) {
    markdown <- getNamespace("roxygen2")[["markdown"]]
    desc <- markdown(desc)
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
#' @typed name,type,description: character(1)
#'   Fields parsed from the `@typed` tag.
#' @param ... Additional arguments unused.
#'
#' @return A formatted character value.
#'
#' @keywords internal
default_format <- function(x, name, type, default = NULL, description, ...) {
  typestr <- type

  # do not wrap code that starts with:
  #  - "[": roxygen link, eg (`[roxygen2::roxy_tag()]`)
  #  - "`": already backticked code
  #  - '"' or "'": quoted strings
  if (!grepl("^[[`'\"]", type)) {
    typestr <- paste0("`", typestr, "`")
  }

  paste0("(", typestr, ") ", description)
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
