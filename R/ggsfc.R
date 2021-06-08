#' Setup a ggsfc plot object
#'
#' @inheritParams sfcurve
#' @param ... Further arguments to be passed to \code{\link{sfcurve}}
#'
#' @return
#' @export
#'
#' @examples
ggsfc <- function(data, ...) {
  UseMethod("ggsfc", data)
}

#'@export
ggsfc.data.frame <- function(data, pos_vars, 
                             curve = c("hilbert", "flowsnake", "sierpinski", "moore"), 
                             order = NULL, len = NULL,
                             limits = NULL, split_by = NULL, 
                             split_arrange = c("interleave", "tile"),
                             ...) {
  
  curve <- match.arg(curve)
  
  envir <- parent.frame()
  
  dat <- sfcurve(data, {{ pos_vars }},
                 curve = curve,
                 order = order,
                 len = len,
                 limits = limits,
                 split_by = split_by,
                 split_arrange = split_arrange)
  
 
  p <- ggplot2::ggplot(dat, environment = envir) +
    ggplot2::coord_equal() +
    theme_curve()
  class(p) <- c('ggsfc', class(p))
  
  p
  
}

#'@export
ggsfc.sfcurve <- function(data, ...) {

  envir <- parent.frame()
  
  p <- ggplot2::ggplot(data, environment = envir) +
    ggplot2::coord_equal() +
    theme_curve()
  class(p) <- c('ggsfc', class(p))
  
  p
  
}