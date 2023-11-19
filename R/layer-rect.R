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
    draw_geom = function(self, data, coord, lineend = "butt", linejoin = "mitre") {
        grid::grid.rect(
            coord$x, coord$y, coord$width, coord$height,
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
)
