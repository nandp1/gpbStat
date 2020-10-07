.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Authors Nandan Patil and Lakshmi Gnagavati")
}

.onLoad <- function(libname, pkgname) {
  options(photobiology.verbose = getOption("verbose"))
}
