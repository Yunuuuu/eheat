layer_text <- function(..., fmt = NULL, check_overlap = FALSE, na.rm = FALSE, matrix = NULL) {
    new_layer(
        geom = eheatTextGeom, ...,
        check_overlap = check_overlap, fmt = fmt,
        na.rm = na.rm,
        matrix = matrix,
        name = "layer_text"
    )
}

eheatTextGeom <- ggplot2::ggproto("eheatTextGeom", eheatGeom,
    required_aes = "label",
    non_missing_aes = "angle",
    optional_aes = character(),
    default_aes = list(
        colour = "black", size = 3.88, angle = 0, hjust = 0.5,
        vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),
    setup_params = function(matrix, params) {
        params$fmt <- allow_lambda(params$fmt)
        params
    },
    draw_geom = function(self, data, fmt = NULL, check_overlap = FALSE, size.unit = "mm") {
        force(data)
        force(fmt)
        force(check_overlap)
        size.unit <- resolve_text_unit(size.unit)
        labels <- data$label
        if (rlang::is_string(fmt)) {
            labels <- sprintf(fmt, labels)
        } else if (is.function(fmt)) {
            labels <- fmt(labels)
        }
        grid::grid.text(labels, data$x, data$y,
            hjust = data$hjust, vjust = data$vjust,
            rot = data$angle,
            gp = gpar(
                col = alpha(data$colour, data$alpha),
                fontsize = data$size * size.unit,
                fontfamily = data$family,
                fontface = data$fontface,
                lineheight = data$lineheight
            ),
            check.overlap = check_overlap,
            name = "eheat_text"
        )
    }
)

resolve_text_unit <- function(unit) {
    unit <- rlang::arg_match0(unit, c("mm", "pt", "cm", "in", "pc"))
    switch(unit,
        "mm" = .pt,
        "cm" = .pt * 10,
        "in" = 72.27,
        "pc" = 12,
        1
    )
}
