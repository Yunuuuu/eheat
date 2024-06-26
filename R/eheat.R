#' Extended Heatmap
#'
#' @param matrix A matrix, if it is a simple vector, it will be converted to a
#' one-column matrix. Data.frame will also be coerced into matrix.
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
#' objects. Check
#' <https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#customize-the-heatmap-body>. You can always use
#' `self` to indicates the matrix attached in this Heatmap.
#' - `layer_fun`: Similar as ``cell_fun``, but is vectorized.
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
#' - `heatmap_height` Height of the whole heatmap (including heatmap components). Check https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#size-of-the-heatmap.
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
#' @param legends_margin,legends_panel A list of
#' [Legends][ComplexHeatmap::Legends-class] objects. `legends_margin` will be
#' added in the `heatmap_legend_list` of
#' [draw][ComplexHeatmap::draw,HeatmapList-method]. `legends_panel` will be
#' plotted in the heatmap matrix panel.Only object with [make_legends] methods
#' can be put in `legends_margin`. Only object with [draw][draw-method] methods
#' can be put in `legends_panel`.
#' @details
#' The initialization function only applies parameter checking and fill values
#' to the slots with some validation.
#'
#' Following methods can be applied to the `Heatmap-class` object:
#'
#' - `show,ExtendedHeatmap-method`: draw a single heatmap with default
#'   parameters
#' - `draw,ExtendedHeatmap-method`: draw a single heatmap.
#' - `+` or `%v%` append heatmaps and annotations to a list of heatmaps.
#'
#' The constructor function pretends to be a high-level graphic function because
#' the ``show`` method of the `Heatmap-class` object actually plots the
#' graphics.
#' @return A `ExtendedHeatmap` Object.
#' @examples
#' eheat(matrix(rnorm(81), nrow = 9))
#' @export
#' @name eheat
eheat <- function(matrix, ...,
                  legends_margin = list(), legends_panel = list()) {
    matrix <- build_matrix(matrix)
    out <- ComplexHeatmap::Heatmap(matrix = matrix, ...)
    out <- methods::as(out, "ExtendedHeatmap")
    out@legends_margin <- legends_margin
    out@legends_panel <- legends_panel
    out
}

#' @importClassesFrom ComplexHeatmap Heatmap
#' @export
#' @rdname eheat
methods::setClass(
    "ExtendedHeatmap",
    slots = list(legends_margin = "list", legends_panel = "list"),
    contains = "Heatmap"
)

#' @examples
#' prepare(eheat(matrix(rnorm(81), nrow = 9)))
#' @inheritParams ComplexHeatmap::prepare
#' @importFrom ComplexHeatmap prepare
#' @export
#' @keywords internal
#' @rdname internal-method
methods::setMethod(
    f = "prepare", signature = "ExtendedHeatmap",
    definition = function(object, process_rows = TRUE, process_columns = TRUE) {
        # `draw,HeatmapList-method` first calls `make_layout,HeatmapList-method`
        # to calculate the layout of the heatmap list and the layout of every
        # single heatmap, then makes the plot by re-calling the graphic
        # functions which are already recorded in the layout.

        # `make_layout,HeatmapList-method` will call `prepare` function for each
        # Heatmap in the HeatmapList:

        # The preparation of the heatmap includes following steps:
        # - making clustering on rows (by calling
        #   `make_row_cluster,Heatmap-method`)
        # - making clustering on columns (by calling
        #   `make_column_cluster,Heatmap-method`)
        # - making the layout of the heatmap (by calling
        #   `make_layout,Heatmap-method`)
        object <- methods::callNextMethod()

        # we merge user-provided legends with ExtendedHeatmap legends
        add_eheat_legends("heatmap_legend_list", object@legends_margin)
        object
    }
)

wrap_heat_fn <- function(object, fun_name) {
    fn <- .subset2(object@matrix_param, fun_name)
    if (is.null(fn)) return(fn) # styler: off
    args <- formals(fn)

    # is.null is a fast path for a common case; the %in% check is slower but
    # also catches the case where there's a `self = NULL` argument.
    if (!is.null(.subset2(args, "self")) || "self" %in% names(args)) {
        matrix <- object@matrix
        function(j, i, x, y, w, h, fill) {
            fn(j, i, x, y, w, h, fill, self = matrix)
        }
    } else {
        fn
    }
}

#' @importClassesFrom ComplexHeatmap HeatmapAnnotation
#' @importFrom ComplexHeatmap make_layout
#' @export
#' @keywords internal
#' @rdname internal-method
methods::setMethod("make_layout", "ExtendedHeatmap", function(object) {
    object <- methods::callNextMethod()

    # run eheat_prepare ---------------------------------
    # New generics to extend ComplexHeatmap easily
    # we can insert legends and modify `draw_fn` here
    object <- eheat_prepare(object)

    # we extend ComplexHeatmap by applying `make_layout` for each annotation
    # and extracted legends
    for (position in c("top", "bottom", "left", "right")) {
        anno_nm <- sprintf("%s_annotation", position)
        annotation <- methods::slot(object, anno_nm)
        if (is.null(annotation)) next
        methods::slot(object, anno_nm) <- make_layout(annotation, object)
    }

    # draw panel legends --------------------------------
    initialized_eheat_fn <- wrap_heat_fn(object, "layer_fun")
    object@matrix_param$cell_fun <- wrap_heat_fn(object, "cell_fun")
    n <- 0L
    total <- length(object@row_order_list) * length(object@column_order_list)
    object@matrix_param$layer_fun <- function(j, i, x, y, w, h, fill) {
        n <<- n + 1L
        if (!is.null(initialized_eheat_fn)) {
            initialized_eheat_fn(j, i, x, y, w, h, fill)
        }
        # in the last slice, we draw panel legends
        if (n == total && length(object@legends_panel)) {
            # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/Heatmap-class.R#L1730
            heatmap_body_vp_name <- sprintf("%s_heatmap_body_wrap", object@name)
            .eheat_decorate(heatmap_body_vp_name, {
                lapply(object@legends_panel, draw)
            })
        }
    }
    object
})

#' @importClassesFrom ComplexHeatmap HeatmapAnnotation
#' @importFrom ComplexHeatmap make_layout
#' @export
#' @keywords internal
#' @rdname internal-method
methods::setMethod(
    "make_layout", "HeatmapAnnotation",
    function(object, heatmap) {
        # we call `make_layout` to initialize ExtendedAnnotation and extract
        # legends
        anno_list <- object@anno_list
        anno_sizes <- object@anno_size
        anno_gaps <- object@gap
        nms <- names(anno_list)
        n_anno <- length(anno_list)
        for (i in seq_along(anno_list)) {
            anno <- anno_list[[i]]@fun
            # if the annotation exits and is `ExtendedAnnotation`
            if (!inherits(anno, "ExtendedAnnotation")) next
            which <- anno@which
            # we allocate each annotation a viewport ----------------
            # always add a viewport for the whole annotation.
            # start from the last annoation which is put on right/bottom
            just <- switch(which,
                row = c(1, 1),
                column = c(0, 1)
            )
            x <- switch(which,
                row = sum(anno_sizes[seq_len(i)]) +
                    sum(anno_gaps[seq_len(i)]) -
                    anno_gaps[i],
                column = unit(0, "npc")
            )
            width <- switch(which,
                row = anno_sizes[i],
                column = unit(1, "npc")
            )
            y <- switch(which,
                row = unit(1, "npc"),
                column = sum(anno_sizes[seq(i, n_anno)]) +
                    sum(anno_gaps[seq(i, n_anno)]) -
                    anno_gaps[n_anno]
            )
            height <- switch(which,
                row = unit(1, "npc"),
                column = anno_sizes[i]
            )
            name <- .subset(nms, i)
            vp <- grid::viewport(
                x = x, y = y, width = width, height = height, just = just,
                name = sprintf("annotation_%s", name)
            )
            anno <- make_layout(
                anno,
                viewport = vp,
                heatmap = heatmap,
                name = name
            )
            anno_list[[i]]@fun <- anno
            # we add annotation legends
            add_eheat_legends("annotation_legend_list", anno@legends_margin)
        }
        object@anno_list <- anno_list
        object
    }
)

# Since ComplexHeatmap currently didn't merge
# https://github.com/jokergoo/ComplexHeatmap/pull/1139
# object@heatmap_legend_list <- c(
#     legend_from_gtable(gt),
#     wrap_legend(object@heatmap_legend_list)
# )
# we'll trace back into `make_layout,HeatmapList` method
# here is the magic
#' @param name "heatmap_legend_list" or "annotation_legend_list"
#' @param legends By calling `make_legends` function.
#' @noRd
add_eheat_legends <- function(name, legends, call_target = "make_layout") {
    if (length(legends) == 0L) return(NULL) # styler: off
    legends <- make_legends(legends)
    pos <- 2L
    nframes <- sys.nframe() - 1L # total parents
    while (pos <= nframes) {
        env <- parent.frame(pos) # we locate the legend environment
        if (is_from_eheat(env) &&
            exists(name, envir = env, inherits = FALSE) &&
            # Since ComplexHeatmap function much are the S4 methods
            # we identify the call name from the parent generic function
            is_call_from(pos, call_target)) {
            old <- wrap_legend(.subset2(env, name))
            index <- grep(
                "^\\.__track__eheat__legends__\\d+$",
                rlang::names2(old),
                perl = TRUE
            )
            old_eheat_legends <- old[index]
            names(legends) <- paste0(
                ".__track__eheat__legends__",
                length(old_eheat_legends) + seq_along(legends)
            )
            # we then modify the legend list
            assign(
                # user provided legends always in the end
                name, c(old_eheat_legends, legends, old[-index]),
                envir = env
            )
            break
        }
        pos <- pos + 1L
    }
}
