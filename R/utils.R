#' If-not-null-else
#'
#' @name if-not-null-else
#' @keywords internal
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs


#' Capture regex groups
#'
#' Captures regex groups and returns a named matrix of groups with one column
#' per capture group and one row per element in `x`.
#'
#' @typed pattern: character[1]
#'   A regex pattern to use for capturing groups.
#' @typed x: character
#'   A vector of strings to match against.
#' @param ... Additional arguments passed to [regexpr()].
#'
#' @keywords internal
regex_capture <- function(pattern, x, ...) {
  match <- regexpr(pattern, x, ...)

  starts <- attr(match, "capture.start")
  lengths <- attr(match, "capture.length")
  group_names <- attr(match, "capture.names")

  match <- substring(x, starts, starts + lengths - 1)

  match <- matrix(match, ncol = length(starts))
  colnames(match) <- group_names

  match
}
