layer_rect <- function(..., lineend = "butt", linejoin = "mitre", na.rm = FALSE, matrix = NULL) {
    new_layer(
        geom = eheatRectGeom, ...,
        linejoin = linejoin,
        lineend = lineend,
        na.rm = na.rm,
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
    draw_slice = function(self, data, group, lineend = "butt", linejoin = "mitre") {
        force(data)
        force(lineend)
        force(linejoin)
        function(j, i, x, y, w, h, fill) {
            data <- match_data(data, i, j)
            grid::grid.rect(x, y,
                width = w, height = h,
                gp = gpar(
                    col = data$colour,
                    fill = alpha(data$fill, data$alpha),
                    lwd = data$linewidth * .pt,
                    lty = data$linetype,
                    linejoin = linejoin,
                    lineend = lineend
                ),
                name = "eheat_rect"
            )
        }
    }
)
