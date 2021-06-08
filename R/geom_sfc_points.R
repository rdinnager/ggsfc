StatSfcPoint <- ggproto("StatSfcPoint", Stat, 
                         
                         compute_panel = function(data, scales) {
                     
                           p_dat <- data %>%
                             dplyr::mutate(pos = purrr::transpose(pos) %>%
                                             dplyr::as_tibble()%>% 
                                             dplyr::mutate(dplyr::across(.fns = unlist))) %>%
                             tidyr::unpack(cols = pos) %>%
                             dplyr::filter(what == "data")
                           
                           p_dat
                           
                         },
                         
                         required_aes = c("pos")
)

#' Stat to draw points on a space-filling curve
#'
#' @export
stat_sfc_point <- function(mapping = NULL, data = NULL, geom = "point", 
                           position = "identity", ..., na.rm = TRUE, 
                           show.legend = NA, 
                           inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatSfcPoint, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      na.rm = na.rm, 
      ...
    )
  )
}

#' @export
geom_sfc_point <- function(mapping = NULL, data = NULL, stat = "sfc_point", 
                           position = "identity", ..., na.rm = TRUE, show.legend = NA, 
                           inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomPoint, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      na.rm = na.rm, 
      ...
    )
  )
}

