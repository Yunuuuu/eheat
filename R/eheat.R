eheat <- function(matrix, ...) {
    dots <- rlang::list2(...)
    layers_idx <- vapply(dots, is_eheat_layer, logical(1L))
    layers <- dots[layers_idx]
    params <- dots[!layers_idx]
    rlang::inject(ComplexHeatmap::Heatmap(
        matrix = matrix,
        rect_gp = gpar(type = "none"),
        !!!params,
        layer_fun = eheat_build(layers, heat_matrix = matrix),
        show_heatmap_legend = FALSE
    ))
}

eheat_build <- function(layers, heat_matrix) {
    if (is.null(layers)) {
        return(NULL)
    }
    fn_list <- .mapply(function(layer, id) {
        id <- if (nzchar(id)) NULL else id
        layer$draw_layer(heat_matrix, id)
    }, list(layers, rlang::names2(layers)), NULL)
    function(j, i, x, y, w, h, fill) {
        lapply(fn_list, function(draw_fn) {
            draw_fn(j, i, x, y, w, h, fill)
        })
    }
}
