#' Configuration
#'
#' Various functions for loading, caching and performing configured behaviors
#' using a user-supplied configuration file.
#'
#' This constant is used as a variable name in the package environment while
#' documentation is being built to avoid constantly parsing configurations
#' during evaluation of each tag.
#'
#' @typed path: character[1]
#'   A file path to use when searching for a config file. Either the file path
#'   to a `DESCRIPTION` or the root path of a package, depending on the context
#'   of the function.
#'
#' @name config_helpers
NULL


#' A name for storing shared config state
#'
#' @importFrom utils packageName
#' @noRd
CONFIG <- paste0(".", utils::packageName(), "_config")  # nolint


#' `roxytypes` Config
#'
#' `roxytypes` exposes a few configuration options for helping to fine-tune your
#' documentation. These are stored as key-values in a `list` in either the
#' `Config/roxytypes` field of your `DESCRIPTION` file, or in a
#' `./man/roxytypes/meta.R` file within your package.
#'
#' The available settings are listed below. Some fields are nested, which are
#' shown by concatenating nested keys using `$`.
#'
#'  * `format`: An optional `glue`-style string, which can assume values for
#'    `name`, `type` and `description`. See `?roxytypes::tags` for
#'    details on the source of each of these strings.
#'
#'  * `verbose`: If `TRUE`, emit extra diagnostic alerts while processing the
#'    package.
#'
#' @typedreturn list
#'   A named list of configured behaviors.
#'
#' @keywords internal
#' @importFrom cli cli_alert_info
config <- function(path = getwd(), refresh = FALSE, cache = TRUE) {
  roxytypes_config <- roxygen2::roxy_meta_get("roxytypes")
  if (!refresh && length(roxytypes_config) > 0)
    return(roxytypes_config)

  config <- config_find_from(path)
  if (isTRUE(config$verbose)) {
    cli::cli_alert_info("Loading {.pkg {utils::packageName()}} config")
  }

  # store roxylint in roxygen2 environment
  roxy_meta_set <- getNamespace("roxygen2")[["roxy_meta_set"]]
  if (cache) roxy_meta_set("roxytypes", config)

  config
}


#' @describeIn config_helpers
#' Load a configuration from a path
#'
#' @keywords internal
config_find_from <- function(path = ".") {
  path <- find_package_root(path)
  if (is.null(path)) return(list())

  config_desc <- config_from_desc(path)
  config_file <- config_from_file(path)

  if (!is.null(config_desc) && !is.null(config_file))
    stop(errors$redundant_config)

  config_desc %||% config_file
}


#' @describeIn config_helpers
#' Load a configuration from a DESCRIPTION file
#'
#' @keywords internal
config_from_desc <- function(path = ".") {
  path <- file.path(path, "DESCRIPTION")

  field <- paste0("Config/", packageName())
  config_desc <- read.dcf(path, fields = field)[1, field]

  result <- tryCatch(
    eval(parse(text = config_desc)),
    error = function(e) stop(errors$description_parse_failure(e$message))
  )

  if (is.na(result)) return(NULL)
  result
}


#' @describeIn config_helpers
#' Load a configuration from a dotfile
#'
#' @keywords internal
#' @importFrom utils packageName
config_from_file <- function(path = ".") {
  pattern <- "^meta\\.[rR]"

  path <- file.path(path, "man", utils::packageName())
  config_files <- list.files(
    path,
    pattern = pattern,
    all.files = TRUE,
    full.names = TRUE
  )

  if (length(config_files) == 0)
    return(NULL)

  res <- new.env()
  source(config_files[[1]], local = res)[[1]]
}
