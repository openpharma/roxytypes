.onLoad <- function(libname, pkgname) {
  if (requireNamespace("roxylint", quietly = TRUE))
    roxylint::register_linters(typed = lint_tidy_typed)
}

lint_tidy_typed <- function(x, name, type, description, ...) {
  roxylint::lint_sentence_case(description)
  roxylint::lint_full_stop(description)
}
