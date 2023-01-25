#' Errors used internally
#'
#' @noRd
errors <- list(
  # Multiple config options were used, likely accidentally
  redundant_config = paste0(
    "Redundant roxytypes configs found:\n",
    " * DESCRIPTION [Config/roxytypes]\n",
    " * ./.roxytypes(.R)"
  ),

  # While parsing a config from DESCRIPTION, a parse or eval failure occured
  description_parse_failure = function(msg) {
    paste0(
      "Could not parse DESCRIPTION contents at Config/", packageName(), "\n",
      msg
    )
  },

  # A tag was unable to be parsed because of a roxygen2 comment syntax error
  parse_syntax = paste0(
    "Failed to parse @typed tag. Tag should be of the form:\n\n",
    "    #' @typed <param>: <type>\n",
    "    #'   <description>\n\n"
  )
)
