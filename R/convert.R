#' Convert `roxygen2` tags to `roxytypes` tags
#'
#' Convert a package codebase into applicable `roxytypes` tags. For `roxygen2`
#' tags with drop-in replacements (namely `@param` and `@return` tags), process
#' descriptions and replace tags with `roxytypes` equivalents.
#'
#' @details
#' A format string is built using [build_format_regex()], which accepts
#' parameters `type` and `description`, which describe how to match these
#' components of a parameter definition. They are combined with the literal
#' content of `format` to produce a regular expression to split existing
#' definitions.
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
#' @typedreturn logical[1]
#'   `TRUE` if successfully completes, `FALSE` if aborted. Always returns
#'   invisibly.
#'
#' @examples
#' \dontrun{
#' convert("(`{type}`) {description}")
#' }
#'
#' @export
convert <- function(
    path = ".",
    format = config(path, refresh = TRUE, cache = FALSE)$format,
    ...,
    unmatched = FALSE,
    verbose = interactive()) {
  # process format to build expression for matching tag decriptions
  format <- build_format_regex(format, ...)

  # build index of all package roxygen tags
  blocks <- roxygen_blocks(path = path, refresh = TRUE, cache = FALSE)
  tags <- unlist(lapply(blocks, `[[`, "tags"), recursive = FALSE)

  # filter ellipsis, they are not converted
  tag_is_ellipsis <- function(t) is.list(t$val) && identical(t$val$name, "...")
  is_ellipsis <- vlapply(tags, tag_is_ellipsis)

  # build modification index
  tags <- tags[!is_ellipsis]
  edits <- build_convert_edits(format, tags, unmatched = unmatched)

  continue <- 3
  if (nrow(edits) > 0 && verbose) {
    repeat {
      preview_convert_edits(edits, n = continue)
      continue <- convert_continue_prompt()
      if (isTRUE(continue)) break # continue with edits
      if (!is.numeric(continue)) {
        return(invisible(FALSE))
      } # abort
    }
  }

  n_edits <- make_convert_edits(edits)
  if (verbose && n_edits > 0) {
    cli::cli_alert_success("{.val {n_edits}} tags converted")
  }

  file_edits <- make_config_edits(path)
  if (verbose && length(file_edits) > 0) {
    cli::cli_alert_success("{.file {file_edits}} updated")
  }

  invisible(TRUE)
}


#' Convert a `roxygen2` tag to `roxytypes` equivalent
#'
#' @typed tag: [roxygen2::roxy_tag()]
#'   A `roxygen2` tag to convert.
#' @inheritParams convert_match_format
#' @param ... Additional arguments unused.
#'
#' @typedreturn `NULL` or [tag_edit()]
#'   If the tag can be converted, a [tag_edit()] is returned, otherwise `NULL`.
#'
#' @family convert
#' @keywords internal
convert_tag <- function(tag, format, ...) {
  UseMethod("convert_tag", structure(list(), class = tag$tag))
}

#' @describeIn convert_tag
#' Default handler for tags that can not be converted.
#'
convert_tag.default <- function(tag, format, ...) {
  NULL
}

#' @describeIn convert_tag
#' Convert `@return` tags, parsing type and description from existing
#' description.
#'
convert_tag.return <- function(tag, format, ...) {
  m <- convert_match_format(tag$val, format)

  desc <- paste0("  ", split_and_trim(m$description), collapse = "\n")
  new <- sprintf("@typedreturn %s\n%s", m$type, desc)
  new <- paste0("#' ", trimws(strsplit(new, "\n")[[1]], which = "right"))

  tag_edit(tag, new, m$matched)
}

#' @describeIn convert_tag
#' Convert `@param` tags, parsing type and description from existing
#' description.
#'
convert_tag.param <- function(tag, format, ...) {
  m <- convert_match_format(tag$val$description, format)

  new_desc <- paste0("  ", split_and_trim(m$description), collapse = "\n")
  new <- sprintf("@typed %s: %s\n%s", tag$val$name, m$type, new_desc)
  new <- paste0("#' ", trimws(strsplit(new, "\n")[[1]], which = "right"))

  tag_edit(tag, new, m$matched)
}


#' Match a conversion format and structure results
#'
#' @typed x: character[1]
#'   Content to match.
#' @typed format: character[1]
#'   A regular expression, optionally containing named capture groups for `type`
#'   and `description`, which will be used for restructuring the tag as a
#'   `roxytypes`-equivalent tag.
#'
#' @typedreturn: list
#'   A named list of `type`, `description` and `matched` fields. `type` and
#'   `description` represent the result of captured groups. If no capture groups
#'   were used, the raw string is used as a description. `matched` is a
#'   `logical[1]` indicating whether the provided format matched against the
#'   provided input.
#'
#' @family convert
#' @keywords internal
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


#' Built a conversion edit
#'
#' @inheritParams convert_tag
#' @typed new: character
#'   The new content used to replace the tag.
#' @typed matched: logical[1]
#'   Whether the content was generated based on a match of a specified format.
#'
#' @typedreturn data.frame
#'   A single-observation dataset representing information for a tag edit. The
#'   `data.frame` row includes variables:
#'
#'   - `file`: (`character[1]`) The source file for the tag.
#'   - `line`: (`integer[1]`) The first line of the tag.
#'   - `n`: (`integer[1]`) The number of lines the tag spans.
#'   - `matched`: (`logical[1]`) Whether the tag matched a specified format.
#'   - `new`: (`list[1](character)`) The new contents to replace the tag.
#'
#' @family convert
#' @keywords internal
tag_edit <- function(tag, new, matched) {
  edit <- data.frame(
    file = tag$file,
    line = tag$line,
    n = length(strsplit(tag$raw, "\n")[[1]]),
    matched = matched
  )

  edit$new <- list(new)
  edit
}


#' Build a collection of conversion edits
#'
#' @inheritParams convert_match_format
#' @typed tags: list(roxy_tag)
#'   A collection of [roxygen2::roxy_tag()] objects to edit. Can include tags
#'   which have no plausible conversion, which will be filtered before returning
#'   edits.
#' @typed unmatched: logical[1]
#'   Whether to make edits to existing tags which can not be matched with the
#'   provided format. If `TRUE`, the existing description is migrated verbatim
#'   to the new tag, without a type provided. The new syntax will produce
#'   warnings when running [roxygen2::roxygenize()], which can be useful tool
#'   for pinpointing tags that need manual fine-tuning for conversion.
#'
#' @typedreturn data.frame
#'   A collection of possible tag edits as produced by [tag_edit()].
#'
#' @family convert
#' @keywords internal
build_convert_edits <- function(format, tags, unmatched = FALSE) {
  edits <- lapply(tags, function(tag, f) convert_tag(tag, f), format)
  edits <- do.call(rbind, Filter(Negate(is.null), edits))

  # if absolutely no edits to be made, return an empty edit frame
  if (is.null(edits)) {
    spoof_tag <- roxygen2::roxy_tag(raw = "", tag = "")
    return(tag_edit(spoof_tag, new = "", matched = FALSE)[c(), ])
  }

  if (!unmatched) {
    edits <- edits[edits[["matched"]], ]
  }

  edits[order(edits$file, edits$line), ]
}


#' Make tag edits
#'
#' @typed edits: data.frame
#'   A collection of edits (one edit per row), as produced by
#'   [tag_edit()].
#'
#' @typedreturn integer[1]
#'   The number of edits that were made.
#'
#' @family convert
#' @keywords internal
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
      text <- append(text, edit[[1, "new"]], after = after)
    }

    writeLines(text, file)
  }

  nrow(edits)
}
