#' Setup a ggsfc plot object
#'
#' @inheritParams sfcurve
#' @param ... Further arguments to be passed to \code{\link{sfcurve}}
#'
#' @return
#' @export
#'
#' @examples
ggsfc <- function(data, curve = c("hilbert", "flowsnake", "sierpinski", "moore"), 
                  order = NULL, ...) {
  
  curve <- match.arg(curve)
  
  envir <- parent.frame()
  
  dat <- sfcurve(curve = curve, order = order, limits = c(0, 1), ...)
  
  curve_interp <- attr(dat, "curve_interp")
  
  if(hasName(data, "x")) {
    colnames(data)[colnames(data) == "x"] <- "x_orig"
  }
  if(hasName(data, "y")) {
    colnames(data)[colnames(data) == "y"] <- "y_orig"
  }
  fake_data_names <- setdiff(names(data), c("x", "y"))
  fake_data <- data.frame(matrix(NA, nrow = nrow(dat), ncol = length(fake_data_names)))
  names(fake_data) <- fake_data_names

  dat <- cbind(dat, fake_data)
  
  attr(dat, "data") <- data
  attr(dat, "curve_interp") <- curve_interp
  
  p <- ggplot(dat, environment = envir) +
    theme_curve()
  class(p) <- c('ggsfc', class(p))
  
  p
  
}