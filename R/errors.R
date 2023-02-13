#' Errors used internally
#'
#' @importFrom utils packageName
#' @noRd
errors <- list(
  # Multiple config options were used, likely accidentally
  redundant_config = paste0(
    "Redundant roxytypes configs found:\n",
    " * DESCRIPTION [Config/", utils::packageName(), "]\n",
    " * ./man/", utils::packageName(), "/meta.R"
  ),

  # While parsing a config from DESCRIPTION, a parse or eval failure occured
  description_parse_failure = function(msg) {
    paste0(
      "Could not parse DESCRIPTION contents at Config/", utils::packageName(), "\n",
      msg
    )
  },

  # A tag was unable to be parsed because of a roxygen2 comment syntax error
  parse_syntax = function(tag) {
    "failed to parse. See ?roxytypes::tags for syntax."
  }
)
