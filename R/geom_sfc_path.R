#' Draw the space-filling curve itself
#'
#' @param ... Further arguments to be passed to \code{\link[ggplot2]{geom_path}}

#' @export
#'
#' @examples
geom_sfc_path <- function(...) {
  geom_path(aes(x, y), ...)
}