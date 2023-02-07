#' Regular expression helpers
#'
#' @name regex_helpers
NULL


#' @describeIn regex_helpers
#' Match within backticks
#'
#' @examples
#' text <- "@param (`test(\")\")`)"
#' pattern <- sprintf("`%s`", re_backticked())
#' m <- regexec(pattern, text, perl = TRUE)
#' regmatches(text, m)[[1]]
#' # [1] "`test(\")\")`"
#'
#' @export
re_backticked <- function() {
  "(?:[^`]|\\\\`)*"
}


#' @describeIn regex_helpers
#' Match any
#'
#' @export
re_any <- function() {
  ".*?"
}
