#' `roxygen2` `@typedreturn` tag
#'
#' The `@typedreturn` tag introduces a syntax for defining return types as a
#' `roxygen2` tag.
#'
#' There are a couple important syntactic features:
#'
#'   * `:` delimiter between the variable name and type.
#'   * `\n` after the type to separate it from the description.
#'
#' @usage
#' #' @typedreturn <type>
#' #'   <description>
#'
#' @name typedreturn
NULL


#' `roxygen2` `@typedreturn` tag parsing
#'
#' Parse a `@typedreturn` tag and return parsed components as value
#'
#' @inheritParams roxygen2::roxy_tag_parse
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @exportS3Method
roxy_tag_parse.roxy_tag_typedreturn <- function(x) {  # nolint
  parsed <- try_parse_typedreturn(x$raw)

  if (is.null(parsed)) {
    er <- "<parse error>"
    roxygen2::warn_roxy_tag(x, errors$parse_syntax(x$tag))
    x$val <- list(type = er, description = er)
    return(x)
  }

  x$val <- as.list(trimws(parsed[1, ]))
  x
}


#' `roxygen2` `@typedreturn` tag rd section population
#'
#' Push typed fields into `@param` section
#'
#' @inheritParams roxygen2::roxy_tag_rd
#'
#' @importFrom glue glue
#' @importFrom roxygen2 roxy_tag_rd
#' @exportS3Method
roxy_tag_rd.roxy_tag_typedreturn <- function(x, base_path, env) {  # nolint
  config <- config()
  format <- config$format %||% default_format

  desc <- if (is.function(format)) {
    do.call(format, append(list(x), x$val))
  } else {
    glue::glue(format, .envir = x$val)
  }

  roxygen2::rd_section("value", desc)
}



#' Parse a raw `@typedreturn` tag
#'
#' @typed raw: character[1]
#'   A raw string from which to try to parse typed parameter definitions.
#'
#' @keywords internal
try_parse_typedreturn <- function(raw) {
  re <- "(?<type>[^\\n]*)\\n(?<description>(?:.|\\n)*)"
  res <- tryCatch(
    regex_capture(re, raw, perl = TRUE),
    error = function(e) NULL
  )

  if (any(nchar(res[, c("description")]) == 0)) {
    return(NULL)
  }

  res
}