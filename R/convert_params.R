#' Regular expression helpers
#'
#' @name regex_helpers
NULL


#' @describeIn regex_helpers
#' Match within backticks
#'
#' @examples
#' text <- "@param (`test(\")\")`)"
#' pattern <- sprintf("`%s`", re_backticked())
#' m <- regexec(pattern, text, perl = TRUE)
#' regmatches(text, m)[[1]]
#' # [1] "`test(\")\")`"
#'
#' @export
re_backticked <- function() {
  "(?:[^`]|\\\\`)*"
}


#' @describeIn regex_helpers
#' Match any
#'
#' @export
re_any <- function() {
  ".*?"
}


#' Convert `@param` definitions to `@typed` parameters
#'
#' @param format An optional `glue`-style format to use to parse types and
#'   descriptions for conversion to `@typed` tags. Available `glue` keywords
#'   include `type` and `description`, as well as any ellipsis arguments.
#' @typed type: character[1]
#'   A regular expression to use for matching a `{type}`
#'   within the format string. Defaults to matching content between backticks.
#' @typed description: character[1]
#'   A regular expression to use for matching a
#'   `{description}` within the format string.
#' @param ... Additional capture groups (unused)
#' @typed path: character[1]
#'   A path in which to look for `roxygen2` documented R scripts.
#' @typed verbose: logical[1]
#'   Whether to use a command line interface to interactively step
#'   through changes.
#'
#' @examples
#' convert_params("(`{type}`) {description}")
#'
#' @export
convert_params <- function(format,
    type = re_backticked(), description = re_any(), unmatched = FALSE, ...,
    path = ".", format_re, verbose = interactive()) {

  keywords <- list(..., type = type, description = description)
  keywords <- mapply(
    function(k, v) glue::glue("(?<{k}>{v})"),
    names(keywords),
    keywords,
    SIMPLIFY = FALSE
  )

  if (!missing(format)) {
    format_re <- escape_non_glue_re(format)
  }

  if (!missing(format_re)) {
    format_re <- glue::glue(format_re, .envir = as.list(keywords))
    format_re <- paste0("(?s)^", format_re, "$")
  } else {
    format_re <- "(?s)^(?<description>.*)$"
  }

  blocks <- roxygen_blocks(refresh = TRUE, cache = FALSE)
  tags <- unlist(lapply(blocks, `[[`, "tags"), recursive = FALSE)

  is_param <- vcapply(tags, `[[`, "tag") == "param"
  tags <- tags[is_param]

  is_ellipsis <- vlapply(tags, function(t) t$val$name == "...")
  tags <- tags[!is_ellipsis]

  if (verbose) {
    continue <- convert_cli(format_re, tags)
    if (!continue) return(invisible(NULL))
  }

  convert_edit_params(format_re, tags, unmatched = unmatched)
}


#' Conversion Helpers
#'
#' Various functions for supporting conversion from standard roxygen tags to
#' `@typed` tags.
#'
#' @typed format: string
#'   A regular expression to use to match existing param definitions.
#' @typed tags: list(roxy_tag)
#'   A selection of [roxygen2::roxy_tag()]s that should be considered or
#'   converted.
#' @typed unmatched: logical[1]
#'   Indicates whether `@param` tags that don't match the expected pattern
#'   should be converted anyways. In such cases, their entire description is
#'   migrated to a new `@typed` description and the new `@typed` tag's type is
#'   left blank.
#'
#' @name convert_helpers
NULL


#' @describeIn convert_helpers
#' Commit edits to `@param` tags
#'
convert_edit_params <- function(format, tags, unmatched = FALSE) {
  # calculate all edits
  edits <- do.call(rbind, lapply(tags, function(tag) {
    new <- convert_param(format, tag)
    edit <- data.frame(
      file = tag$file,
      line = tag$line,
      n = length(strsplit(tag$raw, "\n")[[1]]),
      matched = attr(new, "matched")
    )
    edit$new <- list(new)
    edit
  }))

  # if we don't want to convert unmatched params, filter them from our edits
  if (!unmatched) {
    edits <- edits[edits[["matched"]], ]
  }

  # then perform them collectively by file
  edits <- edits[order(edits$file, edits$line), ]
  edits_by_file <- split(edits, edits$file)
  for (i in seq_along(edits_by_file)) {
    file <- names(edits_by_file[i])
    file_edits <- edits_by_file[[i]]
    text <- readLines(file)

    for (edit_i in rev(seq_len(nrow(file_edits)))) {
      edit <- file_edits[edit_i, ]
      after <- edit$line - 1
      lines <- after + 1:edit$n

      # remove old lines
      text <- text[-lines]

      # add edited lines
      text <- append(text, paste0("#' ", edit[[1, "new"]]), after = after)
    }

    writeLines(text, file)
  }
}


#' @describeIn convert_helpers
#' Display a CLI to preview converted tags before applying changes
#'
#' @return `TRUE` if proceeding with changes, or `FALSE` if aborting changes
#'
convert_cli <- function(format, tags) {
  cli::cli_text("{.emph Examples}\n")

  repeat {
    preview_convert_diffs(format, tags, n = 3)

    cli::cli({
      cli::cli_text("How would you like to proceed?")
      cli::cli_text()

      cli::cli_ol()
      cli::cli_li("Preview a few more [default]")
      cli::cli_li("Abort")
      cli::cli_li("Continue")
      cli::cli_end()

      cli::cli_text()
    })

    res <- readline("Selection: ")

    switch(
      tolower(res),
      "y" = , "3" = return(TRUE),
      "n" = , "q" = , "2" = return(FALSE)
    )

    cli::cli_text()
  }
}


#' @describeIn convert_helpers
#' Convert a single tag to a `@typed` tag
#'
#' Attempt to match the existing `@param` description and use the matched
#' components to populate a new `@typed` tag.
#'
convert_param <- function(format, param) {
  type <- ""
  description <- param$raw

  matches <- regex_capture(format, param$val$description, perl = TRUE)
  matched <- !all(nchar(matches) == 0)
  if (matched) {
    cols <- colnames(matches)
    if ("type" %in% cols) type <- matches[, "type"]
    if ("description" %in% cols) description <- matches[, "description"]
  }

  out <- sprintf("@typed %s: %s\n%s",
    param$val$name,
    type,
    paste0("  ", trimws(strsplit(description, "\n")[[1]]), collapse = "\n")
  )

  out <- trimws(strsplit(out, "\n")[[1]], which = "right")
  attr(out, "matched") <- matched
  out
}


#' @describeIn convert_helpers
#' Preview diffs after applying conversion rules
#'
#' @param tags A list of [roxygen2::roxy_tag()]s to preview
#' @param n The number of tags to preview
#'
#' @return `NULL`
#'
preview_convert_diffs <- function(format, tags, n = 1) {
  i <- sample(seq_along(tags), size = min(length(tags), n))
  for (tag in tags[i]) {
    preview_convert_diff(format, tag)
    cli::cli_text()
  }
}


#' @describeIn convert_helpers
#' Preview diffs after applying conversion rules
#'
preview_convert_diff <- function(format, tag) {
  n <- length(strsplit(tag$raw, "\n")[[1]])
  skip <- tag$line - 1

  raw <- scan(
    tag$file,
    what = character(),
    sep = "\n",
    skip = skip,
    n = n,
    quiet = TRUE
  )

  new <- paste0("#' ", convert_param(format, tag))

  header <- sprintf("%s:%d:%d", basename(tag$file), tag$line, tag$line + n - 1)
  diff <- suppressWarnings(format(cli::diff_chr(raw, new))[-1])  # warnPartial

  cli::cli({
    cli::cli_rule(header, id = "faint")
    cli::cli_verbatim(diff)
  })
}
