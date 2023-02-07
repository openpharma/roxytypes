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


convert_continue_prompt <- function() {
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
    "n" = , "q" = , "2" = return(FALSE),
    return(NA)
  )
}


#' @describeIn convert_helpers
#' Preview diffs after applying conversion rules
#'
#' @param tags A list of [roxygen2::roxy_tag()]s to preview
#' @param n The number of tags to preview
#'
#' @return `NULL`
#'
preview_convert_edits <- function(edits, n = 1) {
  i <- sample(seq_len(nrow(edits)), size = min(nrow(edits), n))

  cli::cli({
    cli::cli_text("{.emph Examples} ({min(n, nrow(edits))} of {nrow(edits)})\n")
    for (i in seq_len(nrow(edits))) {
      preview_convert_edit(edits[i, ])
      cli::cli_text()
    }
  })
}


#' @describeIn convert_helpers
#' Preview diffs after applying conversion rules
#'
preview_convert_edit <- function(edit) {
  skip <- edit$line - 1
  raw <- scan(
    edit$file,
    what = character(),
    sep = "\n",
    skip = skip,
    n = edit$n,
    quiet = TRUE
  )

  # perform diff and calculate new line offsets
  after <- edit$line - 1
  tag_diff <- cli::diff_chr(raw, edit$new[[1]])

  # print diff with header
  cli::cli({
    cli::cli_rule(basename(edit$file))
    cli::cli_verbatim(format_diff_chr(tag_diff, edit$line))
  })
}

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
  over <- cli_nchar > (w <- getOption("width", 80))
  if (any(over)) {
    d_str[over] <- paste0(
      cli::ansi_substring(d_str[over], 1, w - 2 * nchar_lines - 2),
      cli::col_br_black(ellipsis)
    )
  }

  vcapply(seq_along(d_str), function(i) paste0(lines_str[[i]], d_str[[i]]))
}

diff_lines <- function(d) {
  df <- do.call(rbind, lapply(seq_len(nrow(d$lcs)), function(r) {
    with(d$lcs[r, ], {
      n <- length
      cbind(
        old = if (operation != "insert") rep_len(1, n) else rep_len(NA, n),
        new = if (operation != "delete") rep_len(1, n) else rep_len(NA, n)
      )
    })
  }))

  df[, "old"] <- ifelse(is.na(df[, "old"]), NA, cumsum(!is.na(df[, "old"])))
  df[, "new"] <- ifelse(is.na(df[, "new"]), NA, cumsum(!is.na(df[, "new"])))

  df
}
