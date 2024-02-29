#' Build format regular expression
#'
#' Allow `glue`-style formatting using keyworded regular expressions. The
#' original `glue` string (anything that isn't expanded by `glue`) is treated as
#' a string literal, whereas the contents of populated values can be regular
#' expressions, allowing for a more user-friendly way to construct complicated
#' regular expressions.
#'
#' To bypass glue entirely and use a standard regular expression, use
#' `format_re`.
#'
#' The provided regular expression must match all characters from the start of a
#' string to the end. The string also matches using "dot all" syntax, meaning
#' that the `.` expression will also match newline characters.
#'
#' @typed format: character[1]
#'   A `glue`-style format string. Expanded whisker values are used as a
#'   shorthand for capture groups, where ellipsis arguments can be provided
#'   for additional capture group patterns.
#' @typed format_re: character[1]
#'   Alternatively, provide a standard regular expression directly.
#' @param ... Additional arguments provide keyworded capture groups for `format`
#' @typed type: character[1]
#'   A regular expression to use to match a type signature. By default, matches
#'   within backticks.
#' @typed description: character[1]
#'   A regular expression to use to match a parameter description. By default,
#'   matches any string.
#'
#' @typedreturn character[1]:
#'   A regular expression string, built from component sub-expressions.
#'
#' @examples
#' re <- roxytypes:::build_format_regex(
#'   "{as}{any}{bs}",
#'   as = "a+",
#'   bs = "b+",
#'   any = ".*?"
#' )
#'
#' roxytypes:::regex_capture(re, "aaaa\n\nbb", perl = TRUE)
#'
build_format_regex <- function(
    format,
    format_re,
    ...,
    type = re_backticked(),
    description = re_any()) {
  keywords <- list(..., type = type, description = description)
  keywords <- mapply(
    function(k, v) glue::glue("(?<{k}>{v})"),
    names(keywords),
    keywords,
    SIMPLIFY = FALSE
  )

  format_re <- if (!missing(format_re)) {
    format_re
  } else if (!missing(format) && !is.null(format)) {
    format <- escape_non_glue_re(format)
    glue::glue(format, .envir = as.list(keywords))
  } else {
    "(?<description>.*)"
  }

  paste0("(?s)^", format_re, "$")
}


#' @describeIn build_format_regex
#' Match within backticks
#'
#' @examples
#' text <- "@param (`test(\")\")`)"
#'
#' pattern <- sprintf("`%s`", re_backticked())
#'
#' m <- regexec(pattern, text, perl = TRUE)
#' regmatches(text, m)[[1]]
#' # [1] "`test(\")\")`"
#'
#' @export
re_backticked <- function() {
  "(?:[^`]|\\\\`)*"
}


#' @describeIn build_format_regex
#' Match any
#'
#' @export
re_any <- function() {
  ".*?"
}


#' @describeIn build_format_regex
#' Escape all regular expression special characters
#'
#' In addition, avoid escaping `{}`'s that appear to be used as `glue` keywords.
#' Handles only simple cases, and does not handle recusive curly nesting.
#'
#' @typed x: character[1]
#'   A string to escape.
#'
#' @examples
#' # curlies escaped, as this does not appear to be a glue-style usage
#' roxytypes:::escape_non_glue_re(".{1,3}")
#'
#' # curlies not escaped, as this is a glue-style usage
#' roxytypes:::escape_non_glue_re("this is a {test}")
#'
escape_non_glue_re <- function(x) {
  out <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
  # unescape glue-style whisker words (as opposed to regex {n,n} ranges)
  gsub("\\\\\\{(\\w*)\\\\\\}", "{\\1}", out)
}
