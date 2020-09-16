.sfc <- new.env(parent = parent.env(environment()))

utils::globalVariables(c("pos"))

#' @keywords internal
#' @aliases ggsfc-package
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

#'@import ggplot2
.onLoad <- function(libname, pkgname) {
  .sfc$fractalcurve <- memoise::memoise(pracma::fractalcurve)
}
