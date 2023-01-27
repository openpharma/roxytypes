roxygen_blocks <- function() {
  x <- "roxygen_blocks"
  if (exists(x, .state)) return(.state[[x]])
  .state[[x]] <- suppressWarnings(roxygen2::parse_package(env = topenv()))
}


associated_block <- function(file, line) {
  for (block in roxygen_blocks()) {
    for (tag in block$tags) {
      if (tag$line == line && tag$file == file)
        return(block)
    }
  }

  NULL
}
