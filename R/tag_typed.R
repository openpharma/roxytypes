#' `roxygen2` `@typed` tag parsing
#'
#' Parse a `@typed` tag and return parsed components as value
#'
#' @inheritParams roxygen2::roxy_tag_parse
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @exportS3Method
roxy_tag_parse.roxy_tag_typed <- function(x) {  # nolint
  x$val <- as.list(trimws(try_parse_typed(x$raw)[1, ]))
  x$val$description <- gsub("\\s*\\n\\s*", " ", x$val$description)
  x
}


#' `roxygen2` `@typed` tag rd section population
#'
#' Push typed fields into `@param` section
#'
#' @inheritParams roxygen2::roxy_tag_rd
#'
#' @importFrom roxygen2 roxy_tag_rd
#' @exportS3Method
roxy_tag_rd.roxy_tag_typed <- function(x, base_path, env) {  # nolint
  config <- config_load()
  config_perform_checks(config, x)

  fmt <- config$format %||% "(`{type}`) {description}"
  desc <- glue::glue(fmt, .envir = x$val)
  names(desc) <- x$val$name

  roxygen2::rd_section("param", desc)
}


#' Parse a raw `@typed` tag
#'
#' @typed raw: character[1]
#'   A raw string from which to try to parse typed parameter definitions.
#'
#' @keywords internal
try_parse_typed <- function(raw) {
  re <- "(?<name>[^\\n]*):(?<type>[^\\n]*)\\n(?<description>(?:.|\\n)*)"
  tryCatch(
    regex_capture(re, raw, perl = TRUE),
    error = function(e) stop(call. = e$call, errors$parse_syntax)
  )
}
