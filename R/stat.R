StatSFCRaster <- ggproto("StatSpring", Stat, 
                      setup_params = function(data, params) {
                        if(params$curve %in% c("sierpinski", "flowsnake")) {
                          stop("Sorry, only hilbert and moore curves are compatable with
                               raster based plots.")
                        }
                        if(is.null(params$limits)) {
                          params$limits <- range(data$pos)
                        }
                        if(is.null(params$order)) {
                          len <- params$limits[2] - params$limits[1]
                          dists <- len - sapply(1:20, function(x) curve_n_pnts(params$curve, x))
                          dists[dist < 0] <- Inf
                          params$order <- (1:20)[which.min(dists)]
                        }
                        params
                      },
                      setup_data = function(data, params) {
                        data
                      },
                      compute_panel = function(data, scales, limits = range(data$pos), 
                                               curve = c("hilbert", "moore"),
                                               order = NULL) {
                        cols_to_keep <- setdiff(names(data), c("pos"))
                        
                        
                      },
                      required_aes = c("pos")
)