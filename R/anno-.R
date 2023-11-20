new_anno <- function(matrix, draw_fn, yscale,
                     subset_rule = list(), subsettable = TRUE,
                     width = NULL, height = NULL, show_name = TRUE,
                     which = NULL, name = NULL) {
    if (ht_opt$verbose) {
        cli::cli_inform("construct AnnotationFunction with {.fn {name}}")
    }
    anno <- methods::new("AnnotationFunction")
    which <- cheat_which(which)
    anno@which <- which
    anno@fun_name <- name
    anno_size <- anno_width_and_height(which, width, height, unit(1, "cm"))
    anno@width <- anno_size$width
    anno@height <- anno_size$height
    anno@show_name <- show_name
    anno@n <- nrow(matrix)
    anno@data_scale <- yscale
    anno@fun <- draw_fn
    anno@var_env <- new.env(parent = environment(draw_fn))
    anno@subsettable <- subsettable
    anno@subset_rule <- subset_rule
    anno
}

anno_width_and_height <- function(which, width = NULL, height = NULL, default = unit(10, "mm")) {
    # height must be absolute
    params <- list(width = width, height = height)
    if (which == "row") {
        params <- flip_gp(params)
        arg <- "width" # nolint
    } else {
        arg <- "height"
    }
    if (is.null(params$height)) {
        params$height <- default
    } else if (!ComplexHeatmap::is_abs_unit(params$height)) {
        cli::cli_abort(
            "{.arg {arg}} of the annotation can only be an absolute unit."
        )
    }
    if (is.null(params$width)) {
        params$width <- unit(1L, "npc")
    }
    if (which == "row") flip_gp(params) else params
}
