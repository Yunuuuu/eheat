layer_text <- function(..., matrix = NULL, fmt = NULL, check_overlap = FALSE) {
    new_layer(
        geom = eheatTextGeom, ...,
        check_overlap = check_overlap, fmt = fmt,
        matrix = matrix,
        name = "eheat_text"
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
    draw_slice = function(self, layer_matrix, aesthetics, fmt = NULL, check_overlap = FALSE, size.unit = "mm") {
        force(aesthetics)
        force(fmt)
        force(check_overlap)
        size.unit <- resolve_text_unit(size.unit)
        function(j, i, x, y, w, h, fill) {
            aes_list <- lapply(aesthetics, function(aesthetic) {
                pindex(aesthetic, i, j)
            })
            labels <- aes_list$label
            if (rlang::is_string(fmt)) {
                labels <- sprintf(fmt, labels)
            } else if (is.function(fmt)) {
                labels <- fmt(labels)
            }
            grid::grid.text(labels, x, y,
                hjust = aes_list$hjust, vjust = aes_list$vjust,
                rot = aes_list$angle,
                gp = gpar(
                    col = alpha(aes_list$colour, aes_list$alpha),
                    fontsize = aes_list$size * size.unit,
                    fontfamily = aes_list$family,
                    fontface = aes_list$fontface,
                    lineheight = aes_list$lineheight
                ),
                check.overlap = check_overlap,
                name = "eheat_text"
            )
        }
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
