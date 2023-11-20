anno_fn <- function(
    draw_fn, ..., scale = NULL,
    subset_rule = NULL, subsettable = NULL,
    width = NULL, height = NULL, show_name = TRUE,
    which = NULL, matrix = NULL) {
    anno <- new_anno(matrix = matrix, scale = scale)
    anno$draw_anno(draw_fn, ...,
        which = which,
        subsettable = subsettable,
        subset_rule = subset_rule,
        width = width, height = height,
        show_name = show_name
    )
}
