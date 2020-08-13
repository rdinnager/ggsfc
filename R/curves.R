curve_n_pnts <- function(curve = c("hilbert", "flowsnake", "sierpinski", "moore"),
                         order = 6) {
  switch(curve,
         hilbert = 4^order,
         flowsnake = 7^order + 1,
         sierpinski = 4^(order + 1) + 1,
         moore = 4^order * 4,
         NULL)
}

curve_moore <- function(order) {

  pnts <- pracma::fractalcurve(order, "hilbert")

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
  as.data.frame(pracma::fractalcurve(order, "hilbert"))
}

curve_flowsnake <- function(order) {
  as.data.frame(pracma::fractalcurve(order, "flowsnake"))
}

curve_sierpinski <- function(order) {
  as.data.frame(pracma::fractalcurve(order, "sierpinski"))
}


curve_interp <- function(curve_df, pos, limits = range(pos)) {
  curve_len <- nrow(curve_df)
  all_pos <- seq(limits[1], limits[2], length.out = curve_len)
  data.frame(x = approx(all_pos, curve_df$x, xout = pos)$y,
             y = approx(all_pos, curve_df$y, xout = pos)$y)
}
