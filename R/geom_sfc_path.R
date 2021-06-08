StatSfcPath <- ggproto("StatSfcPath", Stat, 
                        
                        compute_panel = function(data, scales) {
                          
                          p_dat <- data %>%
                            dplyr::mutate(pos = purrr::transpose(pos) %>%
                                            dplyr::as_tibble() %>% 
                                            dplyr::mutate(dplyr::across(.fns = unlist))) %>%
                            tidyr::unpack(cols = pos) %>%
                            dplyr::filter(what == "curve")
                          
                          
                          p_dat
                          
                        },
                       
                       required_aes = "pos"
)


#' Draw the space-filling curve itself
#'
#' @param ... Further arguments to be passed to \code{\link[ggplot2]{geom_path}}
#' @export
#'
#' @examples
geom_sfc_path <- function(mapping = NULL, data = NULL, stat = "sfc_path", 
                          position = "identity", ..., arrow = NULL, 
                          lineend = "butt", linejoin = "round", 
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomPath, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      arrow = arrow, 
      lineend = lineend, 
      linejoin = linejoin, 
      na.rm = na.rm, 
      ...
    )
  )
}