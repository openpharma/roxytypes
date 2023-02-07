#' If-not-null-else
#'
#' @name if-not-null-else
#' @keywords internal
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs


vlapply <- function(..., FUN.VALUE = logical(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

vcapply <- function(..., FUN.VALUE = character(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}


split_and_trim <- function(x) {
  trimws(strsplit(x, "\n")[[1]])
}


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


find_package_root <- function(path = ".") {
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) return(path)
    if (dirname(path) == path) break
    path <- dirname(path)
  }

  NULL
}


escape_non_glue_re <- function(x) {
  out <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
  # unescape glue-style whisker words (as opposed to regex {n,n} ranges)
  gsub("\\\\\\{(\\w+)\\\\\\}", "{\\1}", out)
}
