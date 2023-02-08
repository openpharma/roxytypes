#' Fetch `roxygen2` blocks
#'
#' Avoid recomputing `roxygen2`s parsing by saving the blocks after the first
#' tag is hit.
#'
#' @importFrom roxygen2 parse_package env_package
#' @importFrom cli cli_alert_info
#' @keywords internal
roxygen_blocks <- function(path = getwd(), refresh = FALSE, cache = TRUE) {
  x <- "roxytypes-blocks"
  if (!refresh && !is.null(blocks <- roxygen2::roxy_meta_get(x)))
    return(blocks)

  config <- config()
  if (isTRUE(config$verbose)) {
    cli::cli_alert_info("Loading {.pkg roxytypes} namespace cache")
  }

  blocks <- suppressMessages(suppressWarnings({
    roxygen2::parse_package(path = path, env = roxygen2::env_package(path))
  }))

  # store roxylint in roxygen2 environment
  roxy_meta_set <- getNamespace("roxygen2")[["roxy_meta_set"]]
  if (cache) roxy_meta_set(x, blocks)

  blocks
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
      if (tag$line == line && tag$file == file) return(block)
    }
  }

  NULL
}
