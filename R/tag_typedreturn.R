#' `roxygen2` `@typedreturn` tag
#'
#' The `@typedreturn` tag introduces a syntax for defining return types as a
#' `roxygen2` tag.
#'
#' There is one important syntactic features:
#'
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
#' @typedreturn roxygen2 tag
#'   A parsed `roxygen2` `@typedreturn` rd_tag.
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @exportS3Method
roxy_tag_parse.roxy_tag_typedreturn <- function(x) {  # nolint
  parsed <- try_parse_typedreturn(x$raw)

  if (is.null(parsed)) {
    er <- "<parse error>"
    roxygen2::warn_roxy_tag(x, errors$parse_syntax(x$tag))
    x$val <- list(type = er, description = er)
  } else {
    x$val <- as.list(trimws(parsed[1, ]))
  }

  x$val <- with_roxy_field_subclass(x$val)
  x
}


#' `roxygen2` `@typedreturn` tag rd section population
#'
#' Push typed fields into `@param` section
#'
#' @inheritParams roxygen2::roxy_tag_rd
#' @typedreturn [roxygen2::rd_section]
#'   A `roxygen2` `@value` rd_tag with formatted type information.
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
    glue::glue_data(.x = as.list(x$val), format)
  }

  # handle markdown-style formatting using roxygen2 internals
  if (isTRUE(roxygen2::roxy_meta_get("markdown"))) {
    markdown <- getNamespace("roxygen2")[["markdown"]]
    desc <- markdown(desc)
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
