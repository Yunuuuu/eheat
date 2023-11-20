layer_draw <- function(..., na.rm = FALSE, matrix = NULL) {
    new_layer(
        geom = eheatDrawGeom, ...,
        na.rm = na.rm, matrix = matrix,
        name = "layer_point"
    )
}

eheatDrawGeom <- ggplot2::ggproto("eheatPointGeom", eheatGeom,
    setup_params = function(matrix, params) {
        params$draw <- allow_lambda(params$draw)
        params
    },
    draw_geom = function(self, data, draw, ...) {
        draw(data, ...)
    }
)
