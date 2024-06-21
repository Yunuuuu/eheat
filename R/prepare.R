#' Prepare the Heatmap
#' @inherit ComplexHeatmap::prepare
#' @examples
#' prepare(ggheat(matrix(rnorm(81), nrow = 9)))
#' @importFrom ComplexHeatmap prepare
#' @export
#' @method prepare ggHeatmap
#' @rdname prepare
methods::setMethod(
    f = "prepare", signature = "ggHeatmap",
    definition = function(object, process_rows = TRUE, process_columns = TRUE) {
        # `draw,HeatmapList-method` first calls `make_layout,HeatmapList-method`
        # to calculate the layout of the heatmap list and the layout of every
        # single heatmap, then makes the plot by re-calling the graphic
        # functions which are already recorded in the layout.

        # `make_layout` will call `prepare` function for each Heatmap
        # in the HeatmapList:

        # The preparation of the heatmap includes following steps:
        # - making clustering on rows (by calling
        #   `make_row_cluster,Heatmap-method`)
        # - making clustering on columns (by calling
        #   `make_column_cluster,Heatmap-method`)
        # - making the layout of the heatmap (by calling
        #   `make_layout,Heatmap-method`)
        out <- methods::callNextMethod()
        # based on the heatmap layout, we'll merge the underlying
        # ggplot2 object with ComplexHeatmap and extract the legends
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
    # if heatmap has multiple slices
    # we'll use ggplot2 facet system
    if (any(lengths(order_list) > 1L)) {
        with_slice <- TRUE
    } else {
        with_slice <- FALSE
    }
    # backup row and column index and use long-data ----
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

    # prepare slice panels data ------------------------
    slice_list <- cheat_full_slice_index(order_list)

    # prepare data for ggplot2 --------------------------
    coords <- lapply(slice_list, function(data) {
        data$.slice <- sprintf("r%dc%d", data$.slice_row, data$.slice_column)
        # reverse y-axis as ggplot2 and ComplexHeatmap draw in different
        # direction, but we cannot draw anything if we use `scale_y_reverse`, I
        # don't know why?. So we just reverse the values
        data$.row <- reverse_trans(data$.row)
        data
    })
    coords <- do.call(rbind, coords)
    data <- merge(coords, data,
        by = c(".row_index", ".column_index"),
        all = FALSE
    )

    # create the ggplot2 object --------------------
    p <- ggplot(data, aes(.data$.column, .data$.row))

    # special case for rect_gp$type == "none" ------
    if (!identical(rect_gp$type, "none")) {
        p <- p + ggplot2::geom_tile(
            aes(.data$.column, .data$.row, fill = .data$values),
            width = 1L, height = 1L
        ) + ggplot2::labs(fill = object@name)
    }

    # run the user function to modify the ggplot2 object --------
    if (!is.null(object@ggfn)) {
        p <- rlang::inject(object@ggfn(p, !!!object@ggparams))
        if (!ggplot2::is.ggplot(p)) {
            cli::cli_abort(paste(
                "{.arg ggfn} (ggheat: {object@name}) must",
                "return a {.cls ggplot2} object."
            ))
        }
        if (!inherits(p$facet, "FacetNull")) {
            cli::cli_abort(
                "Cannot set facet in {.fn ggfn} (ggheat: {object@name})"
            )
        }
        if (!inherits(p$coordinates, "CoordCartesian")) {
            cli::cli_abort(paste(
                "Only {.fn coord_cartesian} can be used",
                "in {.fn ggfn} (ggheat: {object@name})"
            ))
        }
        if (!is.null(p$scales$get_scales("x"))) {
            cli::cli_warn("will omit x-scale for ggheat: {object@name}")
        }
        if (!is.null(p$scales$get_scales("y"))) {
            cli::cli_warn("will omit y-scale for ggheat: {object@name}")
        }
        p$scales <- p$scales$non_position_scales()
    }

    # prepare scales ------------------------------
    scales <- lapply(c("row", "column"), function(axis) {
        if (axis == "row") {
            fn <- ggplot2::scale_y_continuous
            labels <- row_nms
        } else {
            fn <- ggplot2::scale_x_continuous
            labels <- col_nms
        }
        cols <- sprintf(c(".slice_%s", ".%s", ".%s_index"), axis)
        # prepapre scales for each slice panel
        cheat_scales(coords[cols], labels, scale_fn = fn)
    })
    names(scales) <- c("row", "column")

    # add the scales into the ggplot2 object -------
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

    # Now: we'll fill the ggplot2 object into heatmap body --------
    gt <- ggplot2::ggplotGrob(p)
    layer_fun <- object@matrix_param$layer_fun
    gglayer <- function(j, i, x, y, w, h, fill) {
        # combine `layer_fun` with ggfn
        if (!is.null(layer_fun)) layer_fun(j, i, x, y, w, h, fill)
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
            vp_gt <- gt_trim_zero_grob(gtable::gtable_filter(gt, pattern))
            fit_panel(vp_gt, vp = vp, elements = NULL)
        } else {
            vp_gt <- gt_trim_zero_grob(gt)
            fit_panel(vp_gt, vp = vp, elements = NULL)
        }
    }
    if (!is.null(object@ggfn) || !identical(rect_gp$type, "none")) {
        # if user provided `ggfn` or rect_gp$type is not none,
        # we should do something with `ggfn`
        object@matrix_param$layer_fun <- gglayer
        # we merge user-provided legends with ggplot2 legends
        # Since ComplexHeatmap currently didn't merge
        # https://github.com/jokergoo/ComplexHeatmap/pull/1139
        # object@heatmap_legend_list <- c(
        #     guide_from_gtable(gt),
        #     wrap_legend(object@heatmap_legend_list)
        # )

        # we'll trace back into `make_layout,HeatmapList` method
        add_gg_legend_list("heatmap_legend_list", guide_from_gtable(gt))

        # we always prevent the ComplexHeatmap Heatmap body legend.
        object@heatmap_param$show_heatmap_legend <- FALSE
    }
    object
}

prepare_gganno <- function(object) {
    for (side in c("left", "right", "top", "bottom")) {
        anno_name <- sprintf("%s_annotation", side)
        annotation <- methods::slot(object, anno_name)
        if (is.null(annotation)) next
        # we initialize ggAnnotationFunction and extract legends
        anno_list <- annotation@anno_list
        for (i in seq_along(anno_list)) {
            anno <- anno_list[[i]]@fun
            # if the annotation exits and is `ggAnnotationFunction`
            if (!inherits(anno, "ggAnnotationFunction")) next
            order_list <- switch(anno@which,
                row = object@row_order_list,
                column = object@column_order_list
            )
            # we initialize the ggplot2 object and extract the legends
            anno_list[[i]]@fun <- make_layout(anno, order_list,
                id = names(anno_list)[i]
            )
        }
        methods::slot(object, anno_name)@anno_list <- anno_list
    }
    object
}

# here is the magic
add_gg_legend_list <- function(name, gg_legends, call = quote(make_layout)) {
    if (length(gg_legends) == 0L) return(NULL) # styler: off
    pos <- -2L
    nframes <- -sys.nframe() + 1L # total parents
    while (pos >= nframes) {
        env <- sys.frame(pos) # we locate the legend environment
        if (identical(utils::packageName(topenv(env)), "ComplexHeatmap") &&
            exists(name, envir = env, inherits = FALSE) &&
            # Since ComplexHeatmap function much are the S4 methods
            # we identify the call name from the parent
            identical(sys.call(pos - 1L)[[1L]], call)) {
            old <- wrap_legend(.subset2(env, name))
            index <- grep("^\\.gg_legend\\d+$", rlang::names2(old), perl = TRUE)
            old_gg_legends <- old[index]
            names(gg_legends) <- paste0(
                ".__gg_legend", seq_along(gg_legends) + length(old_gg_legends)
            )
            # we then modify the legend list
            assign(
                # user provided legends always in the end
                name, c(old_gg_legends, gg_legends, old[-index]),
                envir = env
            )
            break
        }
        pos <- pos - 1L
    }
}

wrap_legend <- function(legend) {
    if (length(legend) > 0L && inherits(legend, c("Legends", "grob"))) {
        list(legend)
    } else {
        legend
    }
}
