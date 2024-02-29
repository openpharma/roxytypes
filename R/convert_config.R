#' Tools for modifying configuration files
#'
#' @typed path: character[1]
#'   A file path within the package directory.
#' @typed dcf: data.frame
#'   The result of [read.dcf()] on the package description file.
#' @typed expr: expression
#'   The parsed contents of an `R`-formatted config entry.
#'
#' @name convert_config_helpers
#' @keywords internal
NULL


#' @describeIn convert_config_helpers
#' Make edits to various configuration files
#'
make_config_edits <- function(path) {
  modified <- character()

  root <- find_package_root(path)
  meta_path <- file.path(root, "man", "roxygen", "meta.R")
  roxygen_uses_meta <- file.exists(meta_path)

  dcf_path <- file.path(root, "DESCRIPTION")
  dcf <- dcf_orig <- read_dcf_asis(dcf_path)

  # update description "Config/Needs" section
  dcf <- update_config_needs(dcf)

  # update "Roxygen" section (or meta.R file)
  if (roxygen_uses_meta) {
    edited <- update_config_roxygen_meta(meta_path)
    if (edited) modified <- append(modified, meta_path)
  } else {
    dcf <- update_config_roxygen_desc(dcf)
  }

  if (!identical(dcf_orig, dcf)) {
    write.dcf(dcf, dcf_path, keep.white = colnames(dcf))
    modified <- append(modified, "DESCRIPTION")
  }

  modified
}


#' @describeIn convert_config_helpers
#' Guess the existing dcf indentation
#'
guess_dcf_indent <- function(dcf) {
  newline <- startsWith(as.character(dcf), "\n")
  if (!any(newline)) return(4)
  min(nchar(gsub("^\n+", "", gsub("\\S.*", "", dcf[, newline]))))
}


#' @describeIn convert_config_helpers
#' Update the Needs section of a DESCRIPTION file
#'
update_config_needs <- function(dcf) {
  n <- guess_dcf_indent(dcf)

  col <- "Config/Needs/documentation"
  if (!col %in% colnames(dcf))
    dcf[, col] = ""

  if (!grepl(utils::packageName(), dcf[, col]))
    dcf[, col] = paste0(dcf[, col], "\n", strrep(" ", n), utils::packageName())

  dcf
}


#' @describeIn convert_config_helpers
#' Update the Roxygen man/roxygen/meta.R file
#'
update_config_roxygen_meta <- function(path) {
  expr <- try(parse(path)[[1]], silent = TRUE)
  expr <- update_config_roxygen_expr(expr)

  if (is.null(expr))
    return(FALSE)

  writeLines(deparse(expr), path)
  TRUE
}


#' @describeIn convert_config_helpers
#' Update the Roxygen DESCRIPTION entry
#'
update_config_roxygen_desc <- function(dcf) {
  col <- "Roxygen"

  if (!col %in% colnames(dcf))
    dcf[, col] <- "list()"

  expr <- try(parse(text = dcf[, col])[[1]], silent = TRUE)
  expr <- update_config_roxygen_expr(expr)
  if (is.null(expr)) return(dcf)

  dcf[, col] <- deparse(expr)
  dcf
}


#' @describeIn convert_config_helpers
#' Update a Roxygen config expression
#'
update_config_roxygen_expr <- function(expr) {
  if (inherits(expr, "try-error")) {
    warning(
      "Error encounterd while trying to parse existing Roxygen config. ",
      "Config will not be automatically modified."
    )

    return(NULL)
  }

  msg <- paste0(
    "Roxygen config is too complex to modify automatically. ",
    "Please add 'roxytypes' to your `packages` field manually."
  )

  if (!is.call(expr) || expr[[1]] != "list") {
    warning(msg)
    return(NULL)
  }

  if (!"packages" %in% names(expr)) {
    expr$packages <- quote("roxytypes")
  } else if (is.atomic(expr$packages)) {
    if (expr$packages != "roxytypes")
      expr$packages <- bquote(c(.(expr$packages), "roxytypes"))
  } else if (is.call(expr$packages) && any(expr$packages[[1]] == c("c", "list"))) {
    if (!"roxytypes" %in% eval(expr$packages, envir = baseenv()))
      expr$packages[length(expr$packages) + 1] <- "roxytypes"
  } else {
    warning(msg)
    return(NULL)
  }

  expr
}
