#' Configuration
#'
#' Various functions for loading, caching and performing configured behaviors
#' using a user-supplied configuration file.
#'
#' This constant is used as a variable name in the package environment while
#' documentation is being built to avoid constantly parsing configurations
#' during evaluation of each tag.
#'
#' @typed x: roxy_tag
#'   An internal, intermediate `roxygen2` tag object upon which to evaluate
#'   configured behaviors.
#' @typed config: list(format= )
#'   A list of configuration parameters.
#' @typed e: environment
#'   A package environment used while running `roxygen2`.
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
#'    `name`, `type`, `default` and `description`. See `?roxytypes::tags` for
#'    details on the source of each of these strings.
#'
#'  * `defaults$derive`: Whether to try to derive default values for
#'    documentation. When a function's formal arguments are length-1 atomic
#'    values (or `NULL`), they can be derived for your documentation.
#'
#'  * `defaults$missing`: Either `NULL` or a character value. If a character
#'    value is provided, it is used in documentation as the default value when
#'    no default is defined. If `NULL`, missing default values are not included
#'    in documentation.
#'
#'  * `defaults$warn_undocumented`: If `TRUE`, alerts are raised if defaults are
#'    defined, but not documented.
#'
#'  * `verbose`: If `TRUE`, emit extra diagnostic alerts while processing the
#'    package.
#'
#' @typedreturn list
#'   A named list of configured behaviors.
#'
#' @importFrom cli cli_alert_info
#' @keywords internal
config <- function(path = getwd(), refresh = FALSE, cache = TRUE) {
  roxytypes_config <- roxygen2::roxy_meta_get("roxytypes")
  if (!refresh && !is.null(roxytypes_config))
    return(roxytypes_config)

  config <- config_find_from(getwd())
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
#' @importFrom utils packageName
#' @keywords internal
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
