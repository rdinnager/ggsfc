#' @export
GeomSFCPoints <- ggproto("GeomSFCPoints", Geom,
                         setup_data = function(data, params) {
                           print(names(attributes(data)))
                           
                           real_data <- attr(data, "data")
                           cols_to_keep <- setdiff(names(real_data), c("x", "y"))
                           
                           pos <- (real_data$pos - params$ends)[1] / params$ends[2]
                           
                           new_data <- attr(data, "curve_interp")(pos)
                           
                           data <- cbind(new_data, real_data[ , cols_to_keep])
                           
                           data
                         },
                         setup_params = function(data, params) {
                           
                           print(names(attributes(data)))
                           params
                         },
                         draw_panel = function(data, panel_params, coord,
                                               ends = range(data$pos),
                                               na.rm = TRUE) {
                           
                           if(!has_groups(data)) {
                             GeomPoint$draw_panel(
                               data = data,
                               panel_params = panel_params,
                               coord = coord
                             )
                           }
                           
                           
                         },
                         required_aes = c("pos"),
                         default_aes = aes(
                           colour = "black",
                           size = 0.5
                         ))

#' Draw points on a space-filling curve
#' 
#' Add Description...
#' 
#' @export
#'
#' @examples
geom_sfc_points <- function(mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", ..., ends = range(data$pos),
                            na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {
  
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomSFCPoints, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      ends = ends, 
      na.rm = na.rm, 
      ...
    )
  )
  
}