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
    slice_list <- eheat_full_slice_index(order_list)

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
        # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
        # ggplot2::labs has relative low priorities, so user can provide scale
        #          name to overwrite the name
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
        eheat_scales(coords[cols], labels, scale_fn = fn)
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

    # if user provided `ggfn` or rect_gp$type is not none,
    # we should do something with `ggfn`
    if (is.null(object@ggfn) && identical(rect_gp$type, "none")) {
        return(object)
    }

    # Now: we'll fill the ggplot2 object into heatmap body --------
    gt <- ggplot2::ggplotGrob(p)
    layer_fun <- object@matrix_param$layer_fun # user provided `layer_fun`
    total <- length(order_list[[1L]]) * length(order_list[[2L]])
    n <- 0L
    # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/Heatmap-class.R#L1730
    heatmap_body_vp_name <- sprintf("%s_heatmap_body_wrap", object@name)
    inside_guides <- get_guides(gt, margins = "i")
    # this function will be called by `draw_heatmap_body`
    # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/Heatmap-draw_component.R
    # combine `layer_fun` with `ggfn`, in this ways, the ComplexHeatmap
    # run layer_fun will call ggfun
    layer_fun_call_ggfn <- function(j, i, x, y, w, h, fill) {
        n <<- n + 1L
        if (!is.null(layer_fun)) layer_fun(j, i, x, y, w, h, fill)
        # fill ggplot2 object in the heatmap body
        if (with_slice) {
            kr <- kc <- NULL
            # we trace back the caller environment
            # until the `draw_heatmap_body` function environment
            pos <- 1L
            nframes <- sys.nframe() - 1L # total parents
            while (pos <= nframes) {
                env <- parent.frame(pos)
                if (is_from_eheat(env) &&
                    exists("kr", envir = env, inherits = FALSE) &&
                    exists("kc", envir = env, inherits = FALSE) &&
                    # Since ComplexHeatmap function much are the S4 methods
                    # we identify the call name from the parent
                    is_call_from(pos, "draw_heatmap_body")) {
                    # trace back into `draw_heatmap_body()`
                    # we can also parse grid::current.viewport()$name
                    # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/Heatmap-draw_component.R#L44
                    # and parse name to get kr or kc
                    # -kr Row slice index.
                    # -kc Column slice index.
                    kr <- .subset2(env, "kr")
                    kc <- .subset2(env, "kc")
                    break
                }
                pos <- pos + 1L
            }
            if (is.null(kr)) {
                cli::cli_abort(paste(
                    "{.fn layer_fun} must be run by {.fn draw_heatmap_body}",
                    "to determine the slice number {.field kr} and {.field kc}"
                ))
            }
            pattern <- sprintf("panel-%d-%d", kr, kc)
            .ggfit(gtable::gtable_filter(gt, pattern), "panel", margins = NULL)
        } else {
            .ggfit(gt, "panel", margins = NULL)
        }
        # in the last slice, we draw inside guides
        if (n == total && length(inside_guides)) {
            .eheat_decorate(heatmap_body_vp_name, {
                lapply(inside_guides, grid::grid.draw)
            })
        }
    }
    object@matrix_param$layer_fun <- layer_fun_call_ggfn
    # we merge user-provided legends with ggplot2 legends
    # Since ComplexHeatmap currently didn't merge
    # https://github.com/jokergoo/ComplexHeatmap/pull/1139
    # object@heatmap_legend_list <- c(
    #     legend_from_gtable(gt),
    #     wrap_legend(object@heatmap_legend_list)
    # )
    # we'll trace back into `make_layout,HeatmapList` method
    add_gg_legend_list("heatmap_legend_list", make_legends(gt))

    # we always prevent the ComplexHeatmap Heatmap body legend.
    object@heatmap_param$show_heatmap_legend <- FALSE
    object
}

prepare_gganno <- function(object) {
    for (side in c("left", "right", "top", "bottom")) {
        anno_name <- sprintf("%s_annotation", side)
        annotation <- methods::slot(object, anno_name)
        if (is.null(annotation)) next
        # we initialize ggAnnotationFunction and extract legends
        anno_list <- annotation@anno_list
        nms <- names(anno_list)
        for (i in seq_along(anno_list)) {
            anno <- anno_list[[i]]@fun
            # if the annotation exits and is `ggAnnotationFunction`
            if (!inherits(anno, "ggAnnotationFunction")) next
            order_list <- switch(anno@which,
                row = object@row_order_list,
                column = object@column_order_list
            )
            # we initialize the ggplot2 object and extract the legends
            anno_list[[i]]@fun <- make_layout(
                anno, order_list,
                heat_matrix = object@matrix,
                id = .subset(nms, i)
            )
        }
        methods::slot(object, anno_name)@anno_list <- anno_list
    }
    object
}
