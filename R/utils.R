#' If-not-null-else
#'
#' @name if-not-null-else
#' @keywords internal
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs


#' vapply helpers
#'
#' @param ... Passed to [vapply()].
#' @param FUN.VALUE A prototype signature to use for [vapply()].
#'
#' @name vapply_helpers
#' @keywords internal
NULL


#' @describeIn vapply_helpers
#' Logical vapply
#'
vlapply <- function(..., FUN.VALUE = logical(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}


#' @describeIn vapply_helpers
#' Character vapply
#'
vcapply <- function(..., FUN.VALUE = character(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}


#' Split and trim a string
#'
#' @typed x: character[1]
#'   A string to split into lines and trim.
#'
#' @typedreturn x: character
#'   A character vector of trimed lines.
#'
#' @keywords internal
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

  if (is.null(attr(match, "capture.start")))
    return(matrix(character()))

  starts <- attr(match, "capture.start")
  lengths <- attr(match, "capture.length")
  group_names <- attr(match, "capture.names")

  match <- substring(x, starts, starts + lengths - 1)

  match <- matrix(match, ncol = length(starts))
  colnames(match) <- group_names

  match
}


#' Find package root directory
#'
#' Traces parent directories until we find a pacakge root
#'
#' @typed path: character[1]
#'   A file path within a package.
#'
#' @typedreturn character[1]
#'   The file path to the package root directory.
#'
#' @keywords internal
find_package_root <- function(path = ".") {
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) return(path)
    if (dirname(path) == path) break
    path <- dirname(path)
  }

  NULL
}


#' A helper to reliably read DCF files
#'
#' @typed path: character[1]
#'   A file path to a DESCRIPTION file.
#'
#' @typedreturn data.frame
#'   The result of [read.dcf()].
#'
read_dcf_asis <- function(path) {
  # read once to get all field names
  tmp <- read.dcf(path, keep.white = TRUE, all = TRUE)
  read.dcf(path, keep.white = colnames(tmp), all = TRUE)
}
