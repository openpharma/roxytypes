#' Conversion Helpers
#'
#' Various functions for supporting conversion from standard roxygen tags to
#' `@typed` tags.
#'
#' @typed edit,edits: data.frame
#'   As produced by `convert_edit_df`. Contains the edit source file, starting
#'   line number, number of original lines modified, the new content to insert
#'   and whether the format was matched for the edit. When singular, the
#'   `data.frame` is a single row.
#' @typed n: integer[1]
#'   A number of edits to display.
#' @typed d: [cli::diff_chr()] result
#'   The diff of the original and new tag contents.
#' @typed offset: integer[1]
#'   A line offset for the start of the diff.
#'
#' @name convert_helpers
#' @keywords internal
NULL


#' @describeIn convert_helpers
#' Show a dialog to ask the user how they would like to proceed
#'
convert_continue_prompt <- function() {
  cli::cli({
    cli::cli_text("Continue to modifying files with edits?")
    cli::cli_text()

    cli::cli_dl(c(
      "  y" = "Yes",
      "  n" = "No",
      "  #" = "Preview a few more (default 3)"
    ))

    cli::cli_text()
  })

  res <- readline("Selection: ")

  if (!nchar(res)) return(3)
  if (!is.na(i <- strtoi(res))) return(i)

  switch(
    tolower(res),
    "y" = return(TRUE),
    "n" = , "q" = return(FALSE),
    NULL
  )
}


#' @describeIn convert_helpers
#' Preview diffs after applying conversion rules
#'
#' @return `NULL`
#'
preview_convert_edits <- function(edits, n = 1) {
  rows <- sample(seq_len(nrow(edits)), size = min(nrow(edits), n))

  cli::cli({
    cli::cli_text("{.emph Examples} ({min(n, nrow(edits))} of {nrow(edits)})\n")
    for (row in rows) {
      preview_convert_edit(edits[row, ])
      cli::cli_text()
    }
  })
}


#' @describeIn convert_helpers
#' Preview diffs after applying conversion rules
#'
preview_convert_edit <- function(edit) {
  raw <- scan(
    edit$file,
    what = character(),
    sep = "\n",
    skip = edit$line - 1,
    n = edit$n,
    quiet = TRUE
  )

  # perform diff and calculate new line offsets
  tag_diff <- cli::diff_chr(raw, edit$new[[1]])

  # print diff with header
  cli::cli({
    cli::cli_rule(basename(edit$file))
    cli::cli_verbatim(format_diff_chr(tag_diff, edit$line))
  })
}


#' @describeIn convert_helpers
#' Format a diff object for cli display
#'
format_diff_chr <- function(d, offset) {
  d_str <- suppressWarnings(format(d)[-1])  # suppress arg partial match
  lines_df <- diff_lines(d)
  lines_df <- lines_df - 1 + offset

  # format line offsets
  is_line_na <- is.na(lines_df)
  line_style <- ifelse(
    is.na(lines_df[, 1]), "insert", ifelse(
    is.na(lines_df[, 2]), "delete",
    "match"
  ))
  lines_df[is_line_na] <- ""

  nchar_lines <- max(nchar(lines_df))
  lines_df[] <- sprintf(sprintf("%%%ds", nchar_lines), lines_df)
  lines_str <- apply(lines_df, 1L, paste, collapse = " ")

  i <- line_style == "insert"
  lines_str[i] <- cli::col_green(lines_str[i])
  i <- line_style == "delete"
  lines_str[i] <- cli::col_blue(lines_str[i])
  lines_str <- cli::bg_br_black(lines_str)

  # format terminal overflow to truncate with ellipsis
  ellipsis <- "\u2026"
  cli_nchar <- cli::ansi_nchar(d_str) + 2 * nchar_lines + 1
  over <- cli_nchar > (w <- cli::console_width())
  if (any(over)) {
    d_str[over] <- paste0(
      cli::ansi_substring(d_str[over], 1, w - 2 * nchar_lines - 2),
      cli::col_br_black(ellipsis)
    )
  }

  vcapply(seq_along(d_str), function(i) paste0(lines_str[[i]], d_str[[i]]))
}


#' @describeIn convert_helpers
#' Build a data.frame of old and new line numbers for a diff
#'
diff_lines <- function(d) {
  df <- do.call(rbind, lapply(seq_len(nrow(d$lcs)), function(r) {
    with(d$lcs[r, ], {  # nolint start
      n <- length
      cbind(
        old = if (operation != "insert") rep_len(1, n) else rep_len(NA, n),
        new = if (operation != "delete") rep_len(1, n) else rep_len(NA, n)
      )
    })  # nolint end
  }))

  df[, "old"] <- ifelse(is.na(df[, "old"]), NA, cumsum(!is.na(df[, "old"])))
  df[, "new"] <- ifelse(is.na(df[, "new"]), NA, cumsum(!is.na(df[, "new"])))

  df
}
