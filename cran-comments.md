# roxytypes

## Foreseable CRAN concerns:

### Examples for Non-Exported Functions

All uses of `:::` are to access non-exported functions from within this package.
They are only used for the purpose of providing example code. If possible,
I would like to keep these examples as I find they are a useful resource for
would-be contributors and users filing bug reports.

### Ensuring that Functions do not Write to the User's Home

There is one function (`convert()`) that has the expressed purpose of
modifying a package's code (converting from roxygen `@param` tags to roxytype
`@typed` tags, a drop-in replacement for use with this package). When used
interactively, users are provided with a prompt that displays proposed changes
before any changes are made.

Tests all write to a sub-directory using `tmpfile()` to generate a unique
temporary directory for each test fixture.
