layer_rect <- function(..., lineend = "butt", linejoin = "mitre", matrix = NULL) {
    new_layer(
        geom = eheatRectGeom, ...,
        linejoin = linejoin,
        lineend = lineend,
        matrix = matrix,
        name = "eheat_rect"
    )
}

eheatRectGeom <- ggplot2::ggproto("eheatRectGeom", eheatGeom,
    required_aes = "colour|fill",
    non_missing_aes = character(),
    optional_aes = character(),
    default_aes = list(
        colour = NA, fill = "grey35",
        linewidth = 0.5, linetype = 1, alpha = NA
    ),
    draw_slice = function(self, layer_matrix, aesthetics, lineend = "butt", linejoin = "mitre") {
        force(aesthetics)
        force(lineend)
        force(linejoin)
        function(i, x, y, w, h, fill) {
            aes_list <- lapply(aesthetics, `[`, i)
            grid::grid.rect(x, y,
                width = w, height = h,
                gp = gpar(
                    col = aes_list$colour,
                    fill = alpha(aes_list$fill, aes_list$alpha),
                    lwd = aes_list$linewidth * .pt,
                    lty = aes_list$linetype,
                    linejoin = linejoin,
                    lineend = lineend
                ),
                name = "eheat_rect"
            )
        }
    }
)
