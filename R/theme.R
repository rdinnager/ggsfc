#' @export
theme_curve <- function(base_size = 11, base_family = "", base_line_size = base_size/22, 
                        base_rect_size = base_size/22) {
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    NULL
  )
}