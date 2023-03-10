% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_package_root}
\alias{find_package_root}
\title{Find package root directory}
\usage{
find_package_root(path = ".")
}
\arguments{
\item{path}{(\code{character[1]}) A file path within a package.}
}
\value{
(\code{character[1]}) The file path to the package root directory.
}
\description{
Traces parent directories until we find a pacakge root
}
\keyword{internal}
