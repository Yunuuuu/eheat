#' @importFrom ComplexHeatmap prepare
#' @export
ComplexHeatmap::prepare

#' @export
#' @rdname eHeat
methods::setMethod(
    f = "prepare", signature = "eHeat",
    definition = function(object, process_rows = TRUE, process_columns = TRUE) {
        out <- methods::callNextMethod()
        # as a termporary placeholder in order to only caculate these once
        rect_gp <- out@matrix_param$gp
        order_list <- list(
            row = out@row_order_list,
            column = out@column_order_list
        )
        if (any(lengths(order_list) > 1L)) {
            with_slice <- TRUE
        } else {
            with_slice <- FALSE
        }

        # prepare data for ggplot2
        matrix <- out@matrix
        row_nms <- rownames(matrix)
        col_nms <- colnames(matrix)
        data <- tibble::as_tibble(matrix, .name_repair = "minimal")
        colnames(data) <- seq_len(ncol(data))
        data$.row_index <- seq_len(nrow(data))
        data <- tidyr::pivot_longer(data,
            cols = !dplyr::all_of(".row_index"),
            names_to = ".column_index", values_to = "values"
        )
        data$.column_index <- as.integer(data$.column_index)
        update_data <- dplyr::bind_rows(
            cheat_full_slice_index(order_list),
            .id = ".slice"
        )
        # reverse y-axis as ggplot2 and ComplexHeatmap draw in different
        # direction, but we cannot use scale_y_reverse, I don't know why?
        # It won't draw anything if we use `scale_y_reverse`.
        update_data <- lapply(
            split(update_data, update_data$.slice_row),
            function(subdata) {
                subdata$.row <- reverse_trans(subdata$.row)
                subdata
            }
        )
        update_data <- dplyr::bind_rows(update_data)
        data <- dplyr::inner_join(update_data, data,
            by = c(".row_index", ".column_index")
        )
        p <- ggplot2::ggplot(data, ggplot2::aes(.data$.column, .data$.row))
        if (!identical(rect_gp$type, "none")) {
            p <- p + ggplot2::geom_tile(
                ggplot2::aes(.data$.column, .data$.row,
                    fill = .data$values
                ),
                width = 1L, height = 1L
            )
        }
        if (!is.null(out@ggfn)) {
            p <- rlang::inject(out@ggfn(p, !!!out@ggparams))
            if (!ggplot2::is.ggplot(p)) {
                cli::cli_abort(
                    "{.arg ggfn} must return a {.cls ggplot2} object."
                )
            }
        }
        if (with_slice) {
            scales <- imap(
                c(.row = ".slice_row", .column = ".slice_column"),
                function(x, i) {
                    lapply(split(data, data[[x]]), function(subdata) {
                        n <- max(subdata[[i]])
                        limits <- c(0.5, n + 0.5)
                        breaks <- seq_len(n)
                        if (i == ".row") {
                            # cannot use reverse
                            fn <- ggplot2::scale_y_continuous
                            labels <- row_nms[subdata$.row_index][
                                order(subdata$.row)
                            ]
                        } else {
                            fn <- ggplot2::scale_x_continuous
                            labels <- col_nms[subdata$.column_index][
                                order(subdata$.column)
                            ]
                        }
                        labels <- labels[!duplicated(labels)]
                        do.call(fn, list(
                            limits = limits,
                            breaks = breaks,
                            labels = labels,
                            expand = ggplot2::expansion()
                        ))
                    })
                }
            )
            p <- p + ggplot2::facet_grid(
                rows = ggplot2::vars(.data$.slice_row),
                cols = ggplot2::vars(.data$.slice_column),
                scales = "free", space = "free"
            ) +
                ggh4x::facetted_pos_scales(
                    x = scales$.column, y = scales$.row
                )
        } else {
            xlabels <- col_nms[data$.column_index][order(data$.column)]
            xlabels <- xlabels[!duplicated(xlabels)]
            ylabels <- row_nms[data$.row_index][order(data$.row)]
            ylabels <- ylabels[!duplicated(ylabels)]
            p <- p +
                ggplot2::scale_x_continuous(
                    limits = c(0.5, ncol(matrix) + 0.5),
                    breaks = seq_len(ncol(matrix)),
                    labels = xlabels,
                    expand = ggplot2::expansion()
                ) +
                ggplot2::scale_y_continuous(
                    limits = c(0.5, nrow(matrix) + 0.5),
                    breaks = seq_len(nrow(matrix)),
                    labels = ylabels,
                    expand = ggplot2::expansion()
                )
        }
        gt <- ggplot2::ggplotGrob(p)
        # combine layer_fun with ggfn
        layer_fun <- out@matrix_param$layer_fun
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
                    trim_zero_grob(gtable::gtable_filter(gt, pattern)),
                    vp = vp
                )
            } else {
                fit_panel(trim_zero_grob(gt), vp = vp, elements = NULL)
            }
        }
        if (!(is.null(out@ggfn) && identical(rect_gp$type, "none"))) {
            out@matrix_param$layer_fun <- gglayer
            out@heatmap_legend_list <- c(
                guide_from_gg(p), out@heatmap_legend_list
            )
            out@heatmap_param$show_heatmap_legend <- FALSE
        }
        out
    }
)
