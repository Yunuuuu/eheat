#' Prepare the Heatmap
#' @inherit ComplexHeatmap::prepare
#' @importFrom ComplexHeatmap prepare
#' @export
#' @method prepare ggHeatmap
#' @rdname prepare
methods::setMethod(
    f = "prepare", signature = "ggHeatmap",
    definition = function(object, process_rows = TRUE, process_columns = TRUE) {
        out <- methods::callNextMethod()
        prepare_gganno(prepare_ggheat(out))
    }
)

prepare_ggheat <- function(object) {
    rect_gp <- object@matrix_param$gp
    # To prevent the internal rect drawing of ComplexHeatmap
    object@matrix_param$gp$type <- "none"
    order_list <- list(
        row = object@row_order_list,
        column = object@column_order_list
    )
    if (any(lengths(order_list) > 1L)) {
        with_slice <- TRUE
    } else {
        with_slice <- FALSE
    }
    # backup row and column index and use long-data
    matrix <- object@matrix
    row_nms <- rownames(matrix)
    col_nms <- colnames(matrix)
    data <- as_tibble0(matrix, rownames = NULL)
    colnames(data) <- seq_len(ncol(data))
    data$.row_index <- seq_len(nrow(data))
    data <- tidyr::pivot_longer(data,
        cols = !".row_index",
        names_to = ".column_index",
        values_to = "values"
    )
    data$.column_index <- as.integer(data$.column_index)

    # prepare slice panels
    slice_list <- cheat_full_slice_index(order_list)

    # prepare data for ggplot2 --------------------------
    coords <- lapply(slice_list, function(data) {
        data$.slice <- sprintf(
            "r%dc%d", data$.slice_row, data$.slice_column
        )
        # reverse y-axis as ggplot2 and ComplexHeatmap draw in different
        # direction, but we cannot use scale_y_reverse, I don't know why?
        # It won't draw anything if we use `scale_y_reverse`.
        data$.row <- reverse_trans(data$.row)
        data
    })
    coords <- do.call(rbind, coords)
    data <- merge(coords, data,
        by = c(".row_index", ".column_index"),
        all = FALSE
    )

    p <- ggplot(data, aes(.data$.column, .data$.row))
    if (!identical(rect_gp$type, "none")) {
        p <- p + ggplot2::geom_tile(
            aes(.data$.column, .data$.row, fill = .data$values),
            width = 1L, height = 1L
        ) + ggplot2::labs(fill = object@name)
    }
    if (!is.null(object@ggfn)) {
        p <- rlang::inject(object@ggfn(p, !!!object@ggparams))
        if (!ggplot2::is.ggplot(p)) {
            cli::cli_abort(
                "{.arg ggfn} must return a {.cls ggplot2} object."
            )
        }
    }
    # prepare scales
    scales <- lapply(c("row", "column"), function(i) {
        if (i == "row") {
            # cannot use reverse
            fn <- ggplot2::scale_y_continuous
            labels <- row_nms
        } else {
            fn <- ggplot2::scale_x_continuous
            labels <- col_nms
        }
        cols <- sprintf(c(".slice_%s", ".%s", ".%s_index"), i)
        cheat_scales(coords[cols], labels, scale_fn = fn)
    })
    names(scales) <- c("row", "column")
    if (with_slice) {
        p <- p + ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.slice_row),
            cols = ggplot2::vars(.data$.slice_column),
            scales = "free", space = "free"
        ) +
            ggh4x::facetted_pos_scales(x = scales$column, y = scales$row)
    } else {
        p <- p + scales$column[[1L]] + scales$row[[1L]]
    }
    if (isTRUE(object@debug)) {
        cli::cli_inform(
            "Return {.cls ggplot} object from {.fn ggfn} of {object@name} heatmap"
        )
        rlang::return_from(sys.frame(which = 1L), value = p)
    } else if (is.function(object@debug)) {
        cli::cli_inform("Debug from {.fn ggfn} of {object@name} heatmap")
        object@debug(p)
    }
    gt <- ggplot2::ggplotGrob(p)
    # combine layer_fun with ggfn
    layer_fun <- object@matrix_param$layer_fun
    # ComplexHeatmap::Heatmap will change the function environment of
    # `layer_fun`, we just assign it directly
    gglayer <- function(j, i, x, y, w, h, fill) {
        if (!is.null(layer_fun)) {
            layer_fun(j, i, x, y, w, h, fill)
        }
        # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/Heatmap-draw_component.R
        # trace back into `draw_heatmap_body()`
        draw_body_env <- parent.frame()
        vp <- grid::viewport()
        if (with_slice) {
            # we can also use grid::current.viewport()
            # and parse name to get kr or kc
            # -kr Row slice index.
            # -kc Column slice index.
            kr <- draw_body_env$kr
            kc <- draw_body_env$kc
            pattern <- sprintf("panel-%d-%d", kr, kc)
            fit_panel(
                gt_trim_zero_grob(gtable::gtable_filter(gt, pattern)),
                vp = vp
            )
        } else {
            fit_panel(gt_trim_zero_grob(gt), vp = vp, elements = NULL)
        }
    }
    if (!(is.null(object@ggfn) && identical(rect_gp$type, "none"))) {
        object@matrix_param$layer_fun <- gglayer
        object@heatmap_legend_list <- c(
            guide_from_gtable(gt),
            prepare_legend_list(object@heatmap_legend_list)
        )
        object@heatmap_param$show_heatmap_legend <- FALSE
    }
    object
}

prepare_gganno <- function(object) {
    full_order_list <- list(
        row_order_list = object@row_order_list,
        column_order_list = object@column_order_list
    )
    annotation_legend_list <- NULL
    for (side in c("left", "right", "top", "bottom")) {
        annotation <- methods::slot(object, sprintf("%s_annotation", side))
        if (is.null(annotation)) next
        for (i in seq_along(annotation@anno_list)) {
            anno <- annotation@anno_list[[i]]@fun
            if (!inherits(anno, "ggAnnotationFunction")) next
            anno <- draw_gganno(
                anno, full_order_list, object@matrix,
                id = names(annotation@anno_list)[i]
            )
            annotation@anno_list[[i]]@fun@fun <- anno$draw_fn
            annotation@anno_list[[i]]@fun <- methods::as(
                annotation@anno_list[[i]]@fun, "AnnotationFunction"
            )
            annotation_legend_list <- c(annotation_legend_list, anno$legend)
        }
        methods::slot(object, sprintf("%s_annotation", side)) <- annotation
    }
    object@annotation_legend_list <- c(
        annotation_legend_list,
        prepare_legend_list(object@annotation_legend_list)
    )
    object
}

prepare_legend_list <- function(x) {
    if (length(x) > 0L && inherits(x, c("Legends", "grob"))) {
        x <- list(x)
    }
    x
}
