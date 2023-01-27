#' Fetch `roxygen2` blocks
#'
#' Avoid recomputing `roxygen2`s parsing by saving the blocks after the first
#' tag is hit.
#'
#' @keywords internal
roxygen_blocks <- function() {
  x <- "roxygen_blocks"
  if (exists(x, .state)) return(.state[[x]])
  .state[[x]] <- suppressWarnings(roxygen2::parse_package(env = topenv()))
}


#' Find an associated block
#'
#' Given a file and line, discover which block the tag is associated with
#'
#' @typed file: character[1]
#'   A file path associated with a tag
#' @typed line: integer[1]
#'   A line number in the file where the tag was found
#'
#' @typedreturn [roxygen2::roxy_block()] | NULL
#'   A `roxy_block` if one could be associated, or `NULL` if not.
#'
#' @keywords internal
associated_block <- function(file, line) {
  for (block in roxygen_blocks()) {
    for (tag in block$tags) {
      if (tag$line == line && tag$file == file)
        return(block)
    }
  }

  NULL
}
