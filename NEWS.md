# roxytypes v0.1.2

* Further fixes to `glue::glue` use, a continuation of the fixes in v0.1.1.
  (@dgkf #18)

# roxytypes v0.1.1

* Updates use of `glue::glue`, which previously made use of
  soon-to-be-deprecated behavior. Thanks to @jennybc reverse dependency checks
  and subsequent PR #15.

# roxytypes v0.1.0

* first CRAN release

# roxytypes v0.0.0.9000

* Automatically apply default formatting based on field types. For `type` 
  fields, this will perform a default formatting  (see
  `roxytypes:::as.character.roxy_tag_field_type`). To use another formatting
  style in a config, use `"{unclass(type)}"` (@dgkf #13)

* Improved the stability of some custom formatting syntax, and improved
  documentation for user-provided custom formatting functions. (@dgkf #11)

* Argument order of `convert()` rearranged so that `path` is the first argument.
  `convert()` will attempt to discover `format` from the config of the paackage
  at `path`. (@dgkf #10)
