#' @inherit ComplexHeatmap::AnnotationFunction
#' @param draw_fn A function which defines how to draw the annotation. See
#' [ComplexHeatmap
#' Manual](https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#implement-new-annotation-functions)
#' for details.
#'
#' The function should have three arguments: `index`, `k` and `n` (the names of
#' the arguments can be arbitrary) where `k` and `n` are optional. `index`
#' corresponds to the indices of rows or columns of the heatmap. The value of
#' `index` is not necessarily to be the whole row indices or column indices in
#' the heatmap. It can also be a subset of the indices if the annotation is
#' split into slices according to the split of the heatmap. `index` is
#' reordered according to the reordering of heatmap rows or columns (e.g. by
#' clustering). So, `index` actually contains a list of row or column indices
#' for the current slice after row or column reordering.
#'
#' k corresponds to the current slice and n corresponds to the total number of
#' slices.
#'
#' @param ylim Ranges of data value on the data axis.
#' @param name Name of the annotation, only used for message.
#' @details
#' `new_anno` is similar with
#' [AnnotationFunction][ComplexHeatmap::AnnotationFunction], but `new_anno`
#' won't change the function environment of `draw_fn`. So it's safe to use
#' `new_anno` in pacakge development, particularly when dealing with internal
#' functions in the package namespace that are likely to exist. `@@subsettable`
#' will always be set to `FALSE`. 
#' @examples
#' x <- 1:10
#' anno1 <- new_anno(
#'     n = 10,
#'     draw_fn = function(index, k, n) {
#'         n <- length(index)
#'         pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(0, 10)))
#'         grid.rect()
#'         grid.points(1:n, x[index], default.units = "native")
#'         if (k == 1) grid.yaxis()
#'         popViewport()
#'     },
#'     height = unit(2, "cm")
#' )
#' m <- rbind(1:10, 11:20)
#' ggheat(m, top_annotation = HeatmapAnnotation(foo = anno1))
#' ggheat(m, top_annotation = HeatmapAnnotation(foo = anno1), column_km = 2)
#' @seealso 
#' - [new_anno_subset]
#' - [AnnotationFunction][ComplexHeatmap::AnnotationFunction]
#' @export
new_anno <- function(n, draw_fn, ylim = NULL,
                     width = NULL, height = NULL, show_name = TRUE,
                     which = NULL, name = NULL) {
    # ComplexHeatmap::AnnotationFunction() will change the function
    # environment of `anno@fun`
    # here: we use new_anno instead, in this way, the function in the
    #       package namespace can be used directly
    assert_string(name, null_ok = TRUE)
    name <- name %||% ""
    if (ht_opt$verbose) {
        msg <- "construct AnnotationFunction"
        if (name == "") msg <- paste(msg, "with {.fn {name}}")
        cli::cli_inform(msg)
    }
    anno <- methods::new("AnnotationFunction")
    which <- cheat_which(which)
    anno@which <- which
    anno@fun_name <- name
    anno_size <- anno_width_and_height(which, width, height, unit(1, "cm"))
    anno@width <- anno_size$width
    anno@height <- anno_size$height
    anno@show_name <- show_name
    anno@n <- n
    anno@data_scale <- ylim %||% c(0L, 1L)
    anno@fun <- draw_fn
    anno@var_env <- new.env(parent = environment(draw_fn))
    anno@subsettable <- FALSE
    anno
}

anno_width_and_height <- function(which, width = NULL, height = NULL,
                                  default = unit(10, "mm")) {
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
