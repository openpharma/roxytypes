list(
  format = "[`{type}`] {description}",
  checks = function(name, type, description) {
    if (!endsWith(description, "."))
      warning("Parameter descriptions must end with a period")

    if (!grepl("^[[:upper:]`]", description))
      warning("Parameter descriptions should be 'Sentence case'")
  }
)
