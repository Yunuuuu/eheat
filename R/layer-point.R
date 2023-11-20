layer_point <- function(..., na.rm = FALSE, matrix = NULL) {
    new_layer(
        geom = eheatPointGeom, ...,
        na.rm = na.rm, matrix = matrix,
        name = "layer_point"
    )
}

eheatPointGeom <- ggplot2::ggproto("eheatPointGeom", eheatGeom,
    non_missing_aes = c("size", "shape", "colour"),
    default_aes = list(
        shape = 19, colour = "black", size = 1.5, fill = NA,
        alpha = NA, stroke = 0.5
    ),
    draw_geom = function(self, data) {
        if (is.character(data$shape)) {
            data$shape <- ggplot2::translate_shape_string(data$shape)
        }
        stroke_size <- data$stroke
        stroke_size[is.na(stroke_size)] <- 0
        grid::grid.points(data$x, data$y,
            pch = data$shape,
            gp = gpar(
                col = alpha(data$colour, data$alpha),
                fill = alpha(data$fill, data$alpha),
                # Stroke is added around the outside of the point
                fontsize = data$size * .pt +
                    stroke_size * ggplot2::.stroke / 2,
                lwd = data$stroke * ggplot2::.stroke / 2
            ),
            name = "eheat_point"
        )
    }
)
