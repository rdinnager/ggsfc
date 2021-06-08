curve_n_pnts <- function(curve = c("hilbert", "flowsnake", "sierpinski", "moore"),
                         order = 6) {
  
  curve <- match.arg(curve)
  
  switch(curve,
         hilbert = 4^order,
         flowsnake = 7^order + 1,
         sierpinski = 4^(order + 1) + 1,
         moore = 4^order * 4)
  
}

curve_moore <- function(order) {

  pnts <- .sfc$fractalcurve(order, "hilbert")

  x_grid <- unique(pnts$x)
  x_grid <- x_grid[order(x_grid)]
  x_space <- abs(mean(x_grid - c(x_grid[-1], NA), na.rm = TRUE))

  y_grid <- unique(pnts$y)
  y_grid <- y_grid[order(y_grid)]
  y_space <- abs(mean(y_grid - c(y_grid[-1], NA), na.rm = TRUE))

  moore_pnts <- list(x = c(-pnts$y,
                           -pnts$y,
                           pnts$y + x_space / 4 + 2,
                           pnts$y + x_space / 4 + 2),
                     y = c(pnts$x,
                           pnts$x + y_space / 4 + 2,
                           -pnts$x + y_space / 4 + 2,
                           -pnts$x))

  moore_pnts$x <- scales::rescale(moore_pnts$x, c(-1, 1))
  moore_pnts$y <- scales::rescale(moore_pnts$y, c(-1, 1))

  as.data.frame(moore_pnts)

}

curve_hilbert <- function(order) {
  as.data.frame(.sfc$fractalcurve(order, "hilbert"))
}

curve_flowsnake <- function(order) {
  flowsnake_pnts <- .sfc$fractalcurve(order, "flowsnake")
  flowsnake_pnts$x <- scales::rescale(flowsnake_pnts$x, c(-1, 1))
  flowsnake_pnts$y <- scales::rescale(flowsnake_pnts$y, c(-1, 1))
  as.data.frame(flowsnake_pnts)
}

curve_sierpinski <- function(order) {
  as.data.frame(.sfc$fractalcurve(order, "sierpinski"))
}

#' Make a space-filling curve object
#' 
#' This function creates a space-filling curve of a particular order and type.
#'
#' @param curve Which space-filling curve to make? Must be one of: "hilbert", "flowsnake", 
#' "sierpinski", or "moore". See details for descriptions.
#' @param order What order should the curve be? This controls the "length" of the fractal curve 
#' (e.g. how many points are there in it?). In other words, higher order curves fill the space more.
#' @param len An alternative way to specify order. Instead, specify the "length" of the curve you want.
#' If specified, the order will be determined as the order producing a curve length closest to \code{len}
#' @param limits The values you want to use as the beginning and end of your curve. This will typically
#' be determined by your data. See details for examples.
#'
#' @return An object of class \code{sfcurve}
#' @export
#'
#' @examples
#' sf_curve <- sfcurve("moore", order = 5)
#' plot(sf_curve, col = rainbow(nrow(sf_curve)))
sfcurve <- function(data, pos_vars, 
                    curve = c("hilbert", "flowsnake", "sierpinski", "moore"), order = NULL, len = NULL,
                    limits = NULL, split_by = NULL, 
                    split_arrange = c("interleave", "tile"),
                    interleave_space = 0.5) {
  
  curve <- match.arg(curve)
  split_arrange <- match.arg(split_arrange)
  
  if(is.null(order) && is.null(len)) {
    stop("You must specify either order or n_pts.")
  } 
  
  if(is.null(order) && !is.null(len)) {
    dists <- sqrt((len - sapply(1:20, function(x) curve_n_pnts(curve, x))) ^ 2)
    order <- (1:20)[which.min(dists)]
  }
  
  sf_curve <- switch(curve,
                    hilbert = curve_hilbert(order),
                    flowsnake = curve_flowsnake(order),
                    sierpinski = curve_sierpinski(order),
                    moore = curve_moore(order))
  
  pos_dat <- dplyr::select(data, {{ pos_vars }}) 
  
  if(is.null(limits)) {
    limits <- range(pos_dat)
  }
  
  if(!is.null(split_by)) {
    
    split_groups <- dplyr::select(data, {{ split_by }})
    num_groups <- dplyr::n_distinct(split_groups)
    
    if(split_arrange == "interleave") {
      lines_to_draw <- num_groups - 1L
      curve_sf <- convert_to_sf(sf_curve)
      
      dist_size <- dist(sf_curve[1:2, 1:2]) * interleave_space
      
      dist_lines <- seq(-dist_size, dist_size, length.out = num_groups)
      
      if(num_groups %% 2 != 0) {
        dist_lines <- dist_lines[-ceiling(num_groups / 2)]
      }
      
      new_curves <- purrr::map(dist_lines,
                               ~make_parallel_curve(curve_sf, .x))
      
    }
    
    
    
    
  }
  
  interp <- curve_interp(sf_curve, limits)
  
  pos_dat <- pos_dat %>%
    dplyr::mutate(dplyr::across({{ pos_vars }},
                                ~ interp(.) %>%
                                  dplyr::mutate(what = "data") %>%
                                  purrr::transpose())) %>%
    dplyr::bind_cols(data %>%
                       dplyr::rename_with(~paste0(.x, "_orig"), {{ pos_vars }}))
  
  sf_curve <- sf_curve %>%
    dplyr::mutate(what = "curve") %>%
    purrr::transpose() %>%
    dplyr::tibble(pos = .) %>%
    dplyr::bind_rows(pos_dat)
  
  class(sf_curve) <- c("sfcurve", "data.frame")
  
  sf_curve
  
}

curve_interp <- function(curve_df, limits = range(pos)) {
  curve_len <- nrow(curve_df)
  all_pos <- seq(limits[1], limits[2], length.out = curve_len)
  
  approx_x <- stats::approxfun(all_pos, curve_df$x)
  approx_y <- stats::approxfun(all_pos, curve_df$y)
  
  function(pos) {
    data.frame(x = approx_x(pos),
               y = approx_y(pos))
  }
  
}

#' Plot an sf_curve object
#'
#' @param x An \code{sfcurve} object
#' @param y Not used 
#' @param ... Further argument to be passed to or from other methods.
#'
#' @export
plot.sfcurve <- function(x, y, ...) {
  class(x) <- "data.frame"
  args <- list(...)
  if("col" %in% names(args)) {
    plot(x, type = "n", asp = 1)
    graphics::segments(utils::head(x$x, -1), utils::head(x$y, -1), x$x[-1], x$y[-1], col = args$col)
  } else {
    do.call(plot, c(list(x = x, type = "l", asp = 1), args))
  }
}

#' @importFrom sf st_as_sf
#' @export
sf::st_as_sf

#' @method st_as_sf sfcurve
#' @export
st_as_sf.sfcurve <- function(x, ...) {
  
  y <- sf::st_as_sf(sf::st_as_sfc(list(sf::st_linestring(as.matrix(x)))))
  
  y
  
}

convert_to_sf <- function(x) {
  
  y <- sf::st_as_sf(sf::st_as_sfc(list(sf::st_linestring(as.matrix(x)))))
  
  y
}

make_parallel_curve <- function(x, d) {
  
  x_end <- sf::st_line_sample(x, sample = 1) %>%
    sf::st_as_sf()
  
  x_start <- sf::st_line_sample(x, sample = 0) %>%
    sf::st_as_sf()
  
  buff_line <- sf::st_buffer(x, d, endCapStyle = "FLAT", joinStyle = "MITRE", singleSide = FALSE, mitreLimit = 100)
  
  buff_coords <- sf::st_cast(buff_line, "LINESTRING") %>%
    sf::st_cast("POINT")
  
  par_line <- buff_coords %>%
    sf::st_coordinates()
  
  if(d < 0) {
    curve_end <- which.min(sf::st_distance(buff_coords, x_end)[ , 1])
    par_line <- par_line[(curve_end + 1L):(nrow(par_line) - 1L), ]
  } else {
    curve_start <- which.min(sf::st_distance(buff_coords, x_start)[ , 1])
    par_line <- par_line[(curve_start + 1L):(nrow(par_line) - 1L), ]
  }
  
  par_line <- par_line %>%
    sf::st_linestring() %>%
    list() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf()
  
  par_line
  
}


