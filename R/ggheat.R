#' Extended Heatmap
#'
#' Plot heatmaps layer by layer
#' @param matrix A matrix, if it is a simple vector, it will be converted to a
#' one-column matrix. Data.frame will also be coerced into matrix.
#' @param ggfn A function or formula, accept a initial [ggplot][ggplot2::ggplot]
#' data as the input and must return a [ggplot][ggplot2::ggplot] object.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function with up to
#'   two arguments: `.x` (single argument) or `.x` and `.y` (two arguments). The
#'   `.` placeholder can be used instead of `.x`.  This allows you to create
#'   very compact anonymous functions (lambdas) with up to two inputs.
#'
#' @param ... Other arguments passsed to [Heatmap][ComplexHeatmap::Heatmap].
#' - `name`: Name of the heatmap. By default the heatmap name is used as the
#'  title of the heatmap legend.
#' - `border`: Whether draw border. The value can be logical or a string of
#'   color.
#' - `border_gp`: Graphic parameters for the borders. If you want to set
#' different parameters for different heatmap slices, please consider to use
#' `decorate_heatmap_body`.
#' - `cell_fun`: Self-defined function to add graphics on each cell. Seven
#' parameters will be passed into this function: ``j``, ``i``, ``x``, ``y``,
#' ``width``, ``height``, ``fill`` which are column index, row index in
#' ``matrix``, coordinate of the cell, the width and height of the cell and the
#' filled color. ``x``, ``y``, ``width`` and ``height`` are all `grid::unit`
#' objects.  -layer_fun Similar as ``cell_fun``, but is vectorized. Check
#' <https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#customize-the-heatmap-body>.
#' - `jitter`: Random shifts added to the matrix. The value can be logical or a
#'      single numeric value. It it is ``TRUE``, random values from uniform
#'      distribution between 0 and 1e-10 are generated. If it is a numeric
#'      value, the range for the uniform distribution is (0, ``jitter``). It is
#'      mainly to solve the problem of "Error: node stack overflow" when there
#'      are too many identical rows/columns for plotting the dendrograms.  ADD:
#'      From version 2.5.6, the error of node stack overflow has been fixed, now
#'      this argument is ignored.
#' - `row_title`: Title on the row.
#' - `row_title_side`: Will the title be put on the left or right of the
#'   heatmap?
#' - `row_title_gp`: Graphic parameters for row title.
#' - `row_title_rot`: Rotation of row title.
#' - `column_title` Title on the column.
#' - `column_title_side` Will the title be put on the top or bottom of the
#'   heatmap?
#' - `column_title_gp` Graphic parameters for column title.
#' - `column_title_rot` Rotation of column titles.
#' - `cluster_rows` If the value is a logical, it controls whether to make
#'               cluster on rows. The value can also be a
#'               [hclust][stats::hclust] or a `stats::dendrogram` which already
#'               contains clustering.  Check
#'               <https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#clustering>.
#' - `cluster_row_slices` If rows are split into slices, whether perform
#' clustering on the slice means?
#' - `clustering_distance_rows`: It can be a pre-defined character which is in
#' ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski",
#' "pearson", "spearman", "kendall"). It can also be a function.
#'   - If the function has one argument, the input argument should be a matrix
#' and the returned value should be a `stats::dist` object.
#'   - If the function has two arguments, the input arguments are two vectors
#' and the function calculates distance between these two vectors.
#'
#' - `clustering_method_rows` Method to perform hierarchical clustering, pass to
#'   [hclust][stats::hclust].
#' - `row_dend_side` Should the row dendrogram be put on the left or right of
#'   the heatmap?
#' - `row_dend_width` Width of the row dendrogram, should be a `grid::unit`
#'   object.
#' - `show_row_dend` Whether show row dendrogram?
#' - `row_dend_gp` Graphic parameters for the dendrogram segments. If users
#'  already provide a `stats::dendrogram` object with edges rendered, this
#'  argument will be ignored.  -row_dend_reorder Apply reordering on row
#'  dendrograms. The value can be a logical value or a vector which contains
#'  weight which is used to reorder rows. The reordering is applied by
#'  `stats::reorder.dendrogram`.
#' - `cluster_columns` Whether make cluster on columns? Same settings as
#' ``cluster_rows``.
#' - `cluster_column_slices` If columns are split into slices, whether perform
#'   clustering on the slice means?
#' - `clustering_distance_columns` Same setting as ``clustering_distance_rows``.
#' - `clustering_method_columns` Method to perform hierarchical clustering, pass
#'   to [hclust][stats::hclust].
#' - `column_dend_side` Should the column dendrogram be put on the top or bottom
#'   of the heatmap?
#' - `column_dend_height` height of the column cluster, should be a `grid::unit`
#'   object.
#' - `show_column_dend` Whether show column dendrogram?
#' - `column_dend_gp` Graphic parameters for dendrogram segments. Same settings
#'   as ``row_dend_gp``.
#' - `column_dend_reorder` Apply reordering on column dendrograms. Same settings
#'   as ``row_dend_reorder``.
#' - `row_order` Order of rows. Manually setting row order turns off clustering.
#' - `column_order` Order of column.
#' - `row_labels` Optional row labels which are put as row names in the heatmap.
#' - `row_names_side` Should the row names be put on the left or right of the
#'   heatmap?
#' - `show_row_names` Whether show row names.
#' - `row_names_max_width` Maximum width of row names viewport.
#' - `row_names_gp` Graphic parameters for row names.
#' - `row_names_rot` Rotation of row names.
#' - `row_names_centered` Should row names put centered?
#' - `column_labels` Optional column labels which are put as column names in the
#'   heatmap.
#' - `column_names_side` Should the column names be put on the top or bottom of
#'   the heatmap?
#' - `column_names_max_height` Maximum height of column names viewport.
#' - `show_column_names` Whether show column names.
#' - `column_names_gp` Graphic parameters for drawing text.
#' - `column_names_rot` Rotation of column names.
#' - `column_names_centered` Should column names put centered?
#' - `top_annotation` A [HeatmapAnnotation][ComplexHeatmap::HeatmapAnnotation]
#'   object.
#' - `bottom_annotation` A
#'   [HeatmapAnnotation][ComplexHeatmap::HeatmapAnnotation] object.
#' - `left_annotation` It should be specified by `rowAnnotation`.
#' - `right_annotation` it should be specified by `rowAnnotation`.
#' - `km` Apply k-means clustering on rows. If the value is larger than 1, the
#' heatmap will be split by rows according to the k-means clustering.  For
#' each row slice, hierarchical clustering is still applied with parameters
#' above.
#' - `split` A vector or a data frame by which the rows are split. But if
#' ``cluster_rows`` is a clustering object, ``split`` can be a single
#' number indicating to split the dendrogram by `stats::cutree`.
#' - `row_km` Same as ``km``.
#' - `row_km_repeats` Number of k-means runs to get a consensus k-means
#' clustering. Note if ``row_km_repeats`` is set to more than one, the final
#' number of groups might be smaller than ``row_km``, but this might means the
#' original ``row_km`` is not a good choice.
#' - `row_split` Same as ``split``.
#' - `column_km` K-means clustering on columns.
#' - `column_km_repeats` Number of k-means runs to get a consensus k-means clustering. Similar as ``row_km_repeats``.
#' - `column_split` Split on columns. For heatmap splitting, please refer to https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#heatmap-split .
#' - `gap` Gap between row slices if the heatmap is split by rows. The value should be a `grid::unit` object.
#' - `row_gap` Same as ``gap``.
#' - `column_gap` Gap between column slices.
#' - `show_parent_dend_line` When heatmap is split, whether to add a dashed line to mark parent dendrogram and children dendrograms?
#' - `width` Width of the heatmap body.
#' - `height` Height of the heatmap body.
#' - `heatmap_width` Width of the whole heatmap (including heatmap components)
#' - `heatmap_height` Height of the whole heatmap (including heatmap components). Check https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#size-of-the-heatmap .
#' - `show_heatmap_legend` Whether show heatmap legend?
#' - `heatmap_legend_param` A list contains parameters for the heatmap legends. See `color_mapping_legend,ColorMapping-method` for all available parameters.
#' - `use_raster` Whether render the heatmap body as a raster image. It helps to reduce file size when the matrix is huge. If number of rows or columns is more than 2000, it is by default turned on. Note if ``cell_fun``
#' is set, ``use_raster`` is enforced to be ``FALSE``.
#' - `raster_device` Graphic device which is used to generate the raster image.
#' - `raster_quality` A value larger than 1.
#' - `raster_device_param` A list of further parameters for the selected graphic
#'   device. For raster image support, please check
#'   https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#heatmap-as-raster-image.
#' - `raster_resize_mat` Whether resize the matrix to let the dimension of the
#'  matrix the same as the dimension of the raster image?  The value
#'  can be logical. If it is ``TRUE``, `base::mean` is used to
#'  summarize the sub matrix which corresponds to a single pixel.  The
#'  value can also be a summary function, e.g. `base::max`.
#' - `raster_by_magick` Whether to use `magick::image_resize` to scale the
#'  image.
#' - `raster_magick_filter` Pass to ``filter`` argument of
#' `magick::image_resize`. A character scalar and all possible values are in
#' `magick::filter_types`. The default is ``"Lanczos"``.
#' - `post_fun` A function which will be executed after the heatmap list is
#'   drawn.
#' @param ggparams Other arguments passed to `ggfn`.
#'
#' @details
#' The initialization function only applies parameter checking and fill values to the slots with some validation.
#'
#' Following methods can be applied to the `Heatmap-class` object:
#'
#' - `show,eHeat-method`: draw a single heatmap with default parameters
#' - `draw,eHeat-method`: draw a single heatmap.
#' - `+` or `%v%` append heatmaps and annotations to a list of heatmaps.
#'
#' The constructor function pretends to be a high-level graphic function because
#' the ``show`` method of the `Heatmap-class` object actually plots the
#' graphics.
#' @return A `eHeat` Object.
#' @export
#' @name eHeat
ggheat <- function(matrix, ggfn = NULL, ..., ggparams = list()) {
    matrix <- build_matrix(matrix)
    ggfn <- allow_lambda(ggfn)
    methods::new("eHeat",
        heatmap = ComplexHeatmap::Heatmap(
            matrix = matrix,
            ...,
            show_heatmap_legend = FALSE
        ),
        ggfn = ggfn, ggparams = ggparams
    )
}

methods::setClassUnion("FunctionOrNull", c("function", "NULL"))

#' @importClassesFrom ComplexHeatmap Heatmap
#' @export
#' @rdname eHeat
methods::setClass(
    "eHeat",
    slots = list(
        heatmap = "Heatmap", ggfn = "FunctionOrNull",
        ggparams = "list"
    )
)

#' @importFrom ComplexHeatmap draw
#' @export
ComplexHeatmap::draw

#' @param object A `eHeat` object.
#' @export
#' @method draw eHeat
#' @rdname eHeat
methods::setMethod("draw", "eHeat", function(object, ..., debug = FALSE) {
    heat <- object@heatmap
    matrix <- heat@matrix
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
    layer_fun <- heat@matrix_param$layer_fun
    rect_gp <- heat@matrix_param$gp
    heat@matrix_param$gp$type <- "none"
    # as a termporary placeholder in order to only caculate these once
    env <- new.env() # nolint
    env$estimate_gg <- TRUE
    env$with_slice <- FALSE
    force(debug)
    # ComplexHeatmap::Heatmap will change the function environment of
    # `layer_fun`, we just assign it directly
    gglayer <- function(j, i, x, y, w, h, fill) {
        if (!is.null(layer_fun)) {
            layer_fun(j, i, x, y, w, h, fill)
        }
        # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/Heatmap-draw_component.R
        # trace back into `draw_heatmap_body()`
        draw_body_env <- parent.frame()
        if (env$estimate_gg) {
            order_list <- list(
                row = draw_body_env$object@row_order_list,
                column = draw_body_env$object@column_order_list
            )
            if (any(lengths(order_list) > 1L)) {
                env$with_slice <- TRUE
            }
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
            if (!is.null(object@ggfn)) {
                p <- rlang::inject(object@ggfn(p, !!!object@ggparams))
                if (!ggplot2::is.ggplot(p)) {
                    cli::cli_abort(
                        "{.arg ggfn} must return a {.cls ggplot2} object."
                    )
                }
            }
            if (env$with_slice) {
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
            env$estimate_gg <- FALSE
            if (isTRUE(debug)) {
                rlang::return_from(sys.frame(which = 1L), value = p)
            } else if (is.function(debug)) {
                debug(p)
            }
            env$p <- p
            env$gt <- ggplot2::ggplotGrob(p)
        }
        vp <- grid::viewport()
        if (env$with_slice) {
            # we can also use grid::current.viewport()
            # and parse name to get kr or kc
            # -kr Row slice index.
            # -kc Column slice index.
            kr <- draw_body_env$kr
            kc <- draw_body_env$kc
            pattern <- sprintf("panel-%d-%d", kr, kc)
            fit_panel(
                trim_zero_grob(gtable::gtable_filter(env$gt, pattern)),
                vp = vp
            )
        } else {
            fit_panel(trim_zero_grob(env$gt), vp = vp, elements = NULL)
        }
    }
    if (!(is.null(object@ggfn) && identical(rect_gp$type, "none"))) {
        heat@matrix_param$layer_fun <- gglayer
    }
    draw(heat, ...)
})

#' @importFrom methods show
#' @export
#' @method show eHeat
#' @rdname eHeat
methods::setMethod("show", "eHeat", function(object) {
    draw(object)
})
