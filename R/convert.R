#' Convert `roxygen2` tags to `roxytypes` tags
#'
#' For `roxygen2` tags with drop-in replacements (namely `@param` and `@return`
#' tags), process descriptions and replace tags with `roxytypes` equivalents.
#'
#' @details
#' A format string is built using [build_format_regex()], which accepts
#' parameters `type` and `description`, which describe how to match these
#' components of a parameter definition. They are combined with the literal
#' content of `format` to produce a regular expression to split existing
#' definitions.
#'
#' For alternative styles, use existing helpers provided by
#' [roxytypes::regex_helpers].
#'
#' For comprehensive control, pass `format_re` directly, bypassing expression
#' construction altogether.
#'
#' @typed format: character[1]
#'   A `glue`-style format to use to parse types and descriptions for conversion
#'   to `roxytypes` tags. Available `glue` keywords include `type` and
#'   `description`. By default, `type` will match any string until a closing
#'   backtick and `description` will match any string. See details for more
#'   information.
#' @param ... Additional arguments passed to [build_format_regex()].
#' @typed unmatched: logical[1]
#'   Indicates whether tags that fail to match should still be converted into
#'   `roxytypes` tags. Such conversions may be convenient if you aim to convert
#'   your package holistically, as it will help to flag undocumented parameter
#'   types the next time you re-build your documentation.
#' @typed path: character[1]
#'   A file path within your package. Defaults to the current working directory.
#' @typed verbose: logical[1]
#'   Indicates whether command-line interface should be emitted so that changes
#'   can be reviewed interactively.
#'
#' @return (`NULL`) invisibly.
#'
#' @examples
#' \dontrun{
#' convert("(`{type}`) {description}")
#' }
#'
#' @export
convert <- function(format, ..., unmatched = FALSE, path = ".",
  verbose = interactive()) {

  # process format to build expression for matching tag decriptions
  format <- build_format_regex(format, ...)

  # build index of all package roxygen tags
  blocks <- roxygen_blocks(refresh = TRUE, cache = FALSE)
  tags <- unlist(lapply(blocks, `[[`, "tags"), recursive = FALSE)

  # filter ellipsis, they are not converted
  is_ellipsis <- vlapply(tags, function(t) is.list(t$val) && identical(t$val$name, "..."))

  # build modification index
  tags <- tags[!is_ellipsis]
  edits <- build_convert_edits(format, tags, unmatched = unmatched)

  if (verbose) repeat {
    preview_convert_edits(edits, n = 3)
    continue <- convert_continue_prompt()
    if (isTRUE(!continue)) return(invisible(NULL))  # abort
    if (isTRUE(continue)) break  # continue with edits
  }

  make_convert_edits(edits)
  make_config_edits(path)
}


convert_tag <- function(tag, format, ...) {
  UseMethod("convert_tag", structure(list(), class = tag$tag))
}

convert_tag.default <- function(tag, format, ...) {
  NULL
}

convert_tag.return <- function(tag, format, ...) {
  m <- convert_match_format(tag$val, format)

  desc <- paste0("  ", split_and_trim(m$description), collapse = "\n")
  new <- sprintf("@typedreturn %s\n%s", m$type, desc)
  new <- paste0("#' ", trimws(strsplit(new, "\n")[[1]], which = "right"))

  convert_edit_df(tag, new, m$matched)
}

convert_tag.param <- function(tag, format, ...) {
  m <- convert_match_format(tag$val$description, format)

  new_desc <- paste0("  ", split_and_trim(m$description), collapse = "\n")
  new <- sprintf("@typed %s: %s\n%s", tag$val$name, m$type, new_desc)
  new <- paste0("#' ", trimws(strsplit(new, "\n")[[1]], which = "right"))

  convert_edit_df(tag, new, m$matched)
}

convert_match_format <- function(x, format) {
  res <- list(type = "", description = x, matched = FALSE)

  matches <- regex_capture(format, x, perl = TRUE)
  res$matched <- !all(nchar(matches) == 0)

  if (res$matched) {
    cols <- colnames(matches)
    if ("type" %in% cols) res$type <- matches[, "type"]
    if ("description" %in% cols) res$description <- matches[, "description"]
  }

  res
}


convert_edit_df <- function(tag, new, matched) {
  edit <- data.frame(
    file = tag$file,
    line = tag$line,
    n = length(strsplit(tag$raw, "\n")[[1]]),
    matched = matched
  )

  edit$new <- list(new)
  edit
}


build_format_regex <- function(format, format_re, ...,
  type = re_backticked(), description = re_any()) {

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
  } else {
    format_re <- "(?<description>.*)"
  }

  paste0("(?s)^", format_re, "$")
}


build_convert_edits <- function(format, tags, unmatched = FALSE) {
  edits <- lapply(tags, function(tag, f) convert_tag(tag, f), format)
  edits <- do.call(rbind, Filter(Negate(is.null), edits))

  if (!unmatched) {
    edits <- edits[edits[["matched"]], ]
  }

  edits[order(edits$file, edits$line), ]
}


make_convert_edits <- function(edits) {
  edits_by_file <- split(edits, edits$file)

  for (i in seq_along(edits_by_file)) {
    file <- names(edits_by_file[i])
    file_edits <- edits_by_file[[i]]
    text <- readLines(file)

    # work through edits from the end of the file backwards so that we don't
    # need to worry about line offsets affecting future edits.
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
