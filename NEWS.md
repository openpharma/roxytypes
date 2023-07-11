# roxytypes (development version)

* Improved the stability of some custom formatting syntax, and improved
  documentation for user-provided custom formatting functions. (@dgkf #11)

* Argument order of `convert()` rearranged so that `path` is the first argument.
  `convert()` will attempt to discover `format` from the config of the paackage
  at `path`. (@dgkf #10)
