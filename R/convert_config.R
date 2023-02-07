make_config_edits <- function(path) {
  root <- find_package_root(path)
  dcf_path <- file.path(root, "DESCRIPTION")

  dcf <- read.dcf(dcf_path, keep.white = TRUE, all = TRUE)
  dcf <- read.dcf(dcf_path, keep.white = colnames(dcf), all = TRUE)

  dcf <- update_config_needs(dcf, n)
  dcf <- update_config_roxygen(dcf, root)

  write.dcf(dcf, dcf_path, keep.white = colnames(dcf))
}


guess_dcf_indent <- function(dcf) {
  newline <- startsWith(as.character(dcf), "\n")
  min(nchar(gsub("^\n+", "", gsub("\\S.*", "", dcf[, newline]))))
}


update_config_needs <- function(dcf, n) {
  n <- guess_dcf_indent(dcf)

  col <- "Config/Needs/documenation"
  if (!col %in% colnames(dcf))
    dcf[, col] = ""

  if (!grepl(utils::packageName(), dcf[, col]))
    dcf[, col] = paste0(dcf[, col], "\n", strrep(" ", n), utils::packageName())

  dcf
}

update_config_roxygen <- function(dcf, root) {
  meta_path <- file.path(root, "man", "roxygen", "meta.R")
  roxygen_uses_meta <- file.exists(meta_path)

  if (roxygen_uses_meta) {
    expr <- update_config_roxygen_expr(parse(f)[[1]])
    writeLines(deparse(expr), meta_path)
  } else {
    col <- "Roxygen"
    if (!col %in% colnames(dcf))
      dcf[, col] = "list()"

    expr <- update_config_roxygen_expr(parse(text = dcf[, col])[[1]])
    dcf[, col] <- deparse(expr)
  }

  dcf
}

update_config_roxygen_expr <- function(expr) {
  msg <- paste0(
    "Roxygen config is too complex to modify automatically. ",
    "Please add 'roxytypes' to your `packages` field manually."
  )

  if (!is.call(expr) || expr[[1]] != "list") {
    warning(msg)
  } else if (!"packages" %in% names(expr)) {
    expr$packages <- quote("roxytypes")
  } else if (is.atomic(expr$packages)) {
    expr$packages <- bquote(c(.(expr$packages), "roxytypes"))
  } else if (is.call(expr$packages) && expr$packages[[1]] %in% c("c", "list")) {
    expr$packages <- append(expr$packages, "roxytypes")
  } else {
    warning(msg)
  }

  expr
}
