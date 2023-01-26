list(
  format = "[`{type}`] {description}",
  # checks = function(name, type, description) {
  #   if (!endsWith(description, "."))
  #     warning("Parameter descriptions must end with a period")

  #   if (!grepl("^[[:upper:]`]", description))
  #     warning("Parameter descriptions should be 'Sentence case'")
  # }
  linters = list(
    title = list(
      function(x, ...) {
        rd <- tools::parse_Rd(textConnection(x$val), fragment = TRUE)
        n <- length(rd)

        re <- "^[[:upper:]]"
        if ((attr(rd[[1]], "Rd_tag") == "TEXT") && !grepl(re, rd[[1]])) {
          warning("should start capitalized and be in 'Sentence case'")
          return()
        }

        re <- "\\.\\s*$"
        if ((attr(rd[[n]], "Rd_tag") == "TEXT") && grepl(re, rd[[n]])) {
          warning("should not be punctuated")
          return()
        }
      }
    ),
    typed = list(
      "parameter descriptions must end with a period" = "\\.$",
      function(x, name, type, description, ...) {
        if (!grepl("^[[:upper:]`]", description))
          message("parameter descriptions should be 'Sentence case'")
      }
    )
  )
)
