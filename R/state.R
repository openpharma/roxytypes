#' Clear state object
#'
#' @keywords internal
clear_state <- function() {
  roxy_meta_set <- getNamespace("roxygen2")[["roxy_meta_set"]]
  roxy_meta_set("roxytypes", NULL)
  roxy_meta_set("roxytypes-blocks", NULL)
}
