#' `roxytypes` tags
#'
#' The `@typed` tag introduces a syntax for defining parameter types as a
#' `roxygen2` tag.
#'
#' Be aware that there are a few syntactic requirements:
#'
#'  * `:` delimiter between the variable name and type.
#'  * `\n` after the type to separate it from the description.
#'
#' @usage
#' #' @typed <var>: <type>
#' #'   <description>
#'
#' @section Default `type` Parsing Syntax:
#'
#' The type portion of the `@typed` tag syntax will handle a bit of syntax as
#' special cases.
#'
#'  * `` [type] ``: Types wrapped in brackets, for example
#'    `[roxygen2::roxy_tags()]` will be left as-is, without wrapping the string
#'    in backticks to display as inline code and preserve the native `roxygen2`
#'    reference link.
#'
#'        #' @typed arg: [package::function()]
#'        #'   long form description.
#'
#'  * `` `type` ``: Types wrapped in backticks will be kept as-is. Additional
#'    backticks will not be inserted.
#'
#'        #' @typed arg: `class`
#'        #'   long form description.
#'
#'  * `"type"` or `'type'`: Types wrapped in quotes (either single or double),
#'    will be provided as literal values, removing the surrounding quotation
#'    marks.
#'
#'        #' @typed arg: "`class_a` or `class_b`"
#'        #'   depending on the class of the object provided, either an `"A"`
#'        #'   or a `"B"`.
#'
#' @section Custom `type` Parsing Function:
#'
#' The above defaults are meant to cover most use cases and should be sufficient
#' for all but the most elaborate development practices. If you need to go
#' beyond these default behaviors, you can also provide a parsing function,
#' accepting the parsed roxygen tag as well as the raw contents.
#'
#' The function accepts the [roxygen2::roxy_tag()] produced when parsing the
#' tag, whose `$val` contains fields `name`, `type` and `description`. For
#' convenience, the `$val` contents is unpacked as arguments, though the
#' structure of this tag is liable to change.
#'
#' To implement a `typescript`-style class union syntax,
#'
#'     #' @typed arg: class_a | class_b | class_c
#'     #'   depending on the class of the object provided, either an `"A"`
#'     #'   or a `"B"`.
#'
#' to produce the parameter definition
#'
#'     (`class_a`, `class_c` or `class_b`) depending on the class of the object
#'     provided, either an `"A"`, `"B"` or a `"C"`.
#'
#' we might define the following in `DESCRIPTION` (or in
#' `man/roxytypes/meta.R`).
#'
#'     Config/roxytypes: list(
#'       format = function(tag, ..., name, type, description) {
#'         types <- paste0("`", trimws(strsplit(type, "|", fixed = TRUE)[[1]]), "`")
#'         types <- glue::glue_collapse(types, sep = ", ", last = " or ")
#'         paste0("(", types, ") ", description)
#'       }
#'     )
#'
#' @aliases typed
#' @name tags
NULL


#' `roxygen2` `@typed` tag parsing
#'
#' Parse a `@typed` tag and return parsed components as value
#'
#' @inheritParams roxygen2::roxy_tag_parse
#' @typedreturn roxygen2 tag
#'   A parsed `roxygen2` `@typed` rd_tag.
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @exportS3Method
roxy_tag_parse.roxy_tag_typed <- function(x) {  # nolint
  parsed <- try_parse_typed(x$raw)

  if (is.null(parsed)) {
    er <- "<parse error>"
    roxygen2::warn_roxy_tag(x, errors$parse_syntax(x$tag))
    x$val <- list(name = er, type = er, description = er)
    return(x)
  } else {
    x$val <- as.list(trimws(parsed[1, ]))
  }

  x$val <- with_roxy_field_subclass(x$val)
  x
}


#' `roxygen2` `@typed` tag rd section population
#'
#' Push typed fields into `@param` section
#'
#' @inheritParams roxygen2::roxy_tag_rd
#' @typedreturn [roxygen2::rd_section]
#'   A `roxygen2` "param" rd_section with formatted type information.
#'
#' @importFrom glue glue
#' @importFrom roxygen2 roxy_tag_rd
#' @exportS3Method
roxy_tag_rd.roxy_tag_typed <- function(x, base_path, env) {  # nolint
  config <- config()

  # format typed tag
  format <- config$format %||% default_format
  desc <- if (is.function(format)) {
    do.call(format, append(list(x), x$val))
  } else {
    glue::glue_data(.x = as.list(x$val), format)
  }

  # handle markdown-style formatting using roxygen2 internals
  if (isTRUE(roxygen2::roxy_meta_get("markdown"))) {
    markdown <- getNamespace("roxygen2")[["markdown"]]
    desc <- markdown(desc)
  }

  names(desc) <- x$val$name
  roxygen2::rd_section("param", desc)
}


#' Default formatter for `@typed`
#'
#' Adds special cases for when the type uses other roxygen2 syntax
#'
#' @typed x: [roxygen2::roxy_tag()]
#'   The tag to format.
#' @typed name,type,description: character(1)
#'   Fields parsed from the `@typed` tag.
#' @param ... Additional arguments unused.
#'
#' @return A formatted character value.
#'
#' @export
default_format <- function(x, name, type, description, ...) {
  paste0("(", as.character(type), ") ", as.character(description))
}

#' @export
as.character.roxy_tag_field_type <- function(x, ...) {
  # do not wrap code that starts with `[` (eg [roxygen2::roxy_tag()]) or is
  # backticked (eg `this is all a class name`)
  if (is_bracketed(x) || is_backticked(x)) {
    unclass(x)  # unaffected

  } else if (!is.null(unwrapped_x <- extract_quoted(x))) {
    # unquote literals wrapped in `'` or `"`
    unclass(unwrapped_x)

  } else {
    # otherwise, assume the raw string is a type and wrap in backticks
    paste0("`", x, "`")
  }
}

#' Parse a raw `@typed` tag
#'
#' @typed raw: character[1]
#'   A raw string from which to try to parse typed parameter definitions.
#'
#' @keywords internal
try_parse_typed <- function(raw) {
  re <- paste0(
    "(?<name>[^\\n:]*):",
    "(?<type>[^\\n]*?)",
    "\\n(?<description>(?:.|\\n)*)"
  )

  res <- tryCatch(
    regex_capture(re, raw, perl = TRUE),
    error = function(e) NULL
  )

  if (any(nchar(res[, c("name", "description")]) == 0)) {
    return(NULL)
  }

  res
}
