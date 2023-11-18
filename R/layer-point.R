layer_point <- function(..., matrix = NULL) {
    new_layer(
        geom = eheatPointGeom, ...,
        matrix = matrix,
        name = "eheat_point"
    )
}

eheatPointGeom <- ggplot2::ggproto("eheatPointGeom", eheatGeom,
    non_missing_aes = c("size", "shape", "colour"),
    default_aes = list(
        shape = 19, colour = "black", size = 1.5, fill = NA,
        alpha = NA, stroke = 0.5
    ),
    draw_slice = function(self, layer_matrix, aesthetics) {
        if (is.character(aesthetics$shape)) {
            aesthetics$shape <- ggplot2::translate_shape_string(
                aesthetics$shape
            )
        }
        function(i, x, y, w, h, fill) {
            aes_list <- lapply(aesthetics, `[`, i)
            stroke_size <- aes_list$stroke
            stroke_size[is.na(stroke_size)] <- 0
            grid::grid.points(x, y,
                pch = aes_list$shape,
                gp = gpar(
                    col = alpha(aes_list$colour, aes_list$alpha),
                    fill = alpha(aes_list$fill, aes_list$alpha),
                    # Stroke is added around the outside of the point
                    fontsize = aes_list$size * .pt +
                        stroke_size * ggplot2::.stroke / 2,
                    lwd = aes_list$stroke * ggplot2::.stroke / 2
                ),
                name = "eheat_point"
            )
        }
    }
)
