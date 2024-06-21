#' Build AnnotationFunction Class with ggplot2
#'
#' @details
#' Both `gganno` and `gganno2` perform identical functions, but `gganno` is not
#' compatible with [Heatmap][ComplexHeatmap::Heatmap].  In such cases, only an
#' empty annotation region will be added. On the other hand, `gganno2` can be
#' seamlessly combined with both [Heatmap][ComplexHeatmap::Heatmap] and
#' [ggheat], although legends will not be extracted.
#'
#' @inheritParams ggheat
#' @param ... Additional arguments passed to `ggfn`.
#' @inheritParams ComplexHeatmap::AnnotationFunction
#' @section ggfn:
#'
#' `ggfn` accept a ggplot2 object with a default data and mapping created by
#' `ggplot(data, aes(.data$.x))` / `ggplot(data, ggplot2::aes(y = .data$.y))`.
#' The original matrix will be converted into a data.frame with another 3
#' columns added:
#' - `.slice`: the slice row (which = "row") or column (which = "column")
#'   number.
#' - `.x`/`.y`: indicating the x-axis (or y-axis) coordinates. Don't use
#'   [coord_flip][ggplot2::coord_flip] to flip coordinates as it may disrupt
#'   internal operations.
#' - `.index`: denoting the row index of the original matrix, where rows are
#'   uniformly considered as observations and columns as variables.
#'
#' @inherit ggheat
#' @seealso [draw-ggAnnotationFunction][draw,ggAnnotationFunction-method]
#' @examples
#' draw(gganno(rnorm(10L), function(p) {
#'     p + geom_point(aes(y = V1))
#' }, height = unit(10, "cm"), width = unit(0.7, "npc")))
#' @return A `ggAnnotationFunction` object.
#' @export
#' @name gganno
gganno <- function(matrix, ggfn, ..., which = NULL,
                   width = NULL, height = NULL, debug = FALSE) {
    matrix <- build_matrix(matrix)
    ggfn <- allow_lambda(ggfn)
    debug <- allow_lambda(debug)
    ggparams <- rlang::list2(...)
    out <- new_anno(
        n = nrow(matrix),
        draw_fn = function(index, k, n) NULL,
        ylim = NULL, which = which, width = width, height = height,
        show_name = FALSE, name = "gganno"
    )
    out <- methods::as(out, "ggAnnotationFunction")
    out@matrix <- matrix
    out@ggfn <- ggfn
    out@ggparams <- ggparams
    out@debug <- debug
    out@gginitialized <- FALSE
    out
}

#' @importClassesFrom ComplexHeatmap AnnotationFunction
#' @export
#' @rdname gganno
#' @include ggheat.R
methods::setClass(
    "ggAnnotationFunction",
    slots = list(
        ggfn = "FunctionOrNull",
        ggparams = "list",
        gginitialized = "logical",
        matrix = "matrix",
        debug = "ANY"
    ),
    contains = "AnnotationFunction"
)

#' @importFrom ComplexHeatmap make_layout
methods::setMethod(
    "make_layout", "ggAnnotationFunction",
    function(object, order_list, add_legend = TRUE,
             heat_matrix = NULL, id = NULL) {
        if (!object@gginitialized) {
            if (is.null(heat_matrix) &&
                (is.null(object@matrix) || is.function(object@matrix))) {
                if (is.null(id)) {
                    fn_id <- "{.fn ggfn}"
                } else {
                    fn_id <- sprintf("{.fn ggfn} (gganno: %s)", id)
                }
                cli::cli_abort(paste(
                    "You must provide a matrix in", fn_id,
                    "in order to draw {.cls ggAnnotationFunction} directly"
                ))
            }
            if (is.null(object@matrix)) {
                object@matrix <- switch(object@which,
                    row = heat_matrix,
                    column = t(heat_matrix)
                )
            } else if (is.function(object@matrix)) {
                data <- switch(object@which,
                    row = heat_matrix,
                    column = t(heat_matrix)
                )
                object@matrix <- object@matrix(data)
            }
            gganno_element <- draw_gganno(
                object, order_list,
                id = id %||% "draw-gganno"
            )

            # we merge the annotation_legend_list with ggplot2 legends -----
            # we'll trace back into `make_layout,HeatmapList` method
            add_gg_legend_list("annotation_legend_list", gganno_element$legend)
            object@fun <- gganno_element$draw_fn
            object@gginitialized <- TRUE
        }
        object
    }
)

#' Draw the ggAnnotationFunction Object
#'
#' @param object The [ggAnnotationFunction][gganno] object.
#' @param index A vector of indices.
#' @param k The current slice index for the annotation if it is split.
#' @param n Total number of slices.
#' @param ... Additional arguments passed on to
# [draw-AnnotationFunction][ComplexHeatmap::draw,HeatmapAnnotation-method].
#' @return draw the annotation.
#' @examples
#' draw(gganno(rnorm(10L), function(p) {
#'     p + geom_point(aes(y = V1))
#' }, height = unit(10, "cm"), width = unit(0.7, "npc")))
#' @export
methods::setMethod(
    f = "draw",
    signature = "ggAnnotationFunction",
    definition = function(object, index, k = 1L, n = 1L, ...) {
        if (ht_opt$verbose) {
            cli::cli_inform("annotation generated by {.fn {object@fun_name}}")
        }
        if (missing(index)) index <- seq_len(object@n)
        if (k == 1L && !object@gginitialized) {
            # This is only used by ComplexHeatmap::Heatmap function
            # since `ggheat` will initialize `gganno` when preparing the main
            # heatmap layout.
            order_list <- gganno_get_order_list("ht_main", object@which)
            if (is.null(order_list)) {
                if (n == 1L) {
                    order_list <- list(index)
                } else {
                    cli::cli_abort(
                        "Cannot initialize {.cls ggAnnotationFunction}"
                    )
                }
            }
            object <- make_layout(object, order_list)
        }
        methods::callNextMethod(
            object = object, index = index,
            k = k, n = n, ...
        )
    }
)

# https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/HeatmapList-draw_component.R#L670
# trace back into `draw_heatmap_list()`
# get slice informations from the draw function
gganno_get_order_list <- function(name, axis, call = quote(draw_heatmap_list)) {
    pos <- -2L
    nframes <- -sys.nframe() + 1L # total parents
    while (pos >= nframes) {
        env <- sys.frame(pos)
        if (identical(utils::packageName(topenv(env)), "ComplexHeatmap") &&
            exists(name, envir = env, inherits = FALSE) &&
            identical(sys.call(pos - 1L)[[1L]], call)) {
            obj <- .subset2(env, name)
            if (methods::.hasSlot(obj, "row_order_list") &&
                methods::.hasSlot(obj, "column_order_list")) {
                return(switch(axis,
                    row = obj@row_order_list,
                    column = obj@column_order_list
                ))
            }
        }
        pos <- pos - 1L
    }
    NULL
}

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
draw_gganno <- function(anno, order_list, heat_matrix, id) {
    if (is.null(id)) {
        fn_id <- "{.fn ggfn}"
    } else {
        id <- sprintf("(gganno: %s)", id)
        fn_id <- sprintf("{.fn ggfn} %s", id)
    }
    which <- anno@which
    # we always regard matrix row as the observations
    matrix <- anno@matrix %||% switch(which,
        row = heat_matrix,
        column = t(heat_matrix)
    )
    data <- as_tibble0(matrix, rownames = NULL) # nolint
    if (length(order_list) > 1L) {
        with_slice <- TRUE
    } else {
        with_slice <- FALSE
    }
    coords <- data_frame0(
        .slice = rep(
            seq_along(order_list),
            times = lengths(order_list)
        ),
        .index = unlist(order_list, recursive = FALSE, use.names = FALSE),
        .x = seq_along(.data$.index)
    )
    data <- cbind(coords, data[match(coords$.index, seq_len(nrow(data))), ])
    if (which == "row") {
        data <- rename(data, c(.x = ".y"))
        if (with_slice) {
            data <- lapply(split(data, data$.slice), function(subdata) {
                subdata$.y <- reverse_trans(subdata$.y)
                subdata
            })
            data <- do.call(rbind, data)
            data <- as_tibble0(data, rownames = NULL)
        } else {
            data$.y <- reverse_trans(data$.y)
        }
        p <- ggplot(data, aes(y = .data$.y))
    } else {
        p <- ggplot(data, aes(x = .data$.x))
    }
    p <- rlang::inject(anno@ggfn(p, !!!anno@ggparams))
    if (!ggplot2::is.ggplot(p)) {
        cli::cli_abort(
            sprintf("%s must return a {.cls ggplot2} object.", fn_id)
        )
    }
    if (!inherits(p$facet, "FacetNull")) {
        cli::cli_abort(sprintf("Cannot set facet in %s", fn_id))
    }
    if (!inherits(p$coordinates, "CoordCartesian")) {
        cli::cli_abort(paste(
            "Only {.fn coord_cartesian} can be used in", fn_id
        ))
    }
    # prepare scales --------------------------------------
    labels <- rownames(matrix) %||% ggplot2::waiver()
    if (which == "row") {
        facet_params <- list(
            rows = ggplot2::vars(.data$.slice),
            scales = "free_y", space = "free_y"
        )
        y_scale <- cheat_scales(coords[c(1L, 3:2)], labels,
            scale_fn = ggplot2::scale_y_continuous
        )
        if (!is.null(p$scales$get_scales("y"))) {
            cli::cli_warn(
                null_paste("will omit y-scale for row annotation", id)
            )
        }
        if (!is.null(x_scale <- p$scales$get_scales("x"))) { # from user
            # avoid the warning message: Attempting to add facetted x
            # scales, while x scales are not free.
            facet_params$scales <- "free"
        }
        # we always omit the position scales
        p$scales <- p$scales$non_position_scales()
    } else {
        facet_params <- list(
            cols = ggplot2::vars(.data$.slice),
            scales = "free_x", space = "free_x"
        )
        x_scale <- cheat_scales(coords[c(1L, 3:2)], labels,
            scale_fn = ggplot2::scale_x_continuous
        )
        if (!is.null(p$scales$get_scales("x"))) {
            cli::cli_warn(
                null_paste("will omit x-scale for column annotation", id)
            )
        }
        if (!is.null(y_scale <- p$scales$get_scales("y"))) { # from user
            # avoid the warning message: Attempting to add facetted y
            # scales, while y scales are not free.
            facet_params$scales <- "free"
        }
        p$scales <- p$scales$non_position_scales()
    }
    # add scales into ggplot2 object ---------------------
    if (with_slice) {
        p <- p + do.call(ggplot2::facet_grid, facet_params)
        p <- p + ggh4x::facetted_pos_scales(x = x_scale, y = y_scale)
    } else {
        # it's safe to add `NULL` or a `list`
        p <- p + x_scale + y_scale
    }
    if (isTRUE(anno@debug)) {
        cli::cli_inform(null_paste(
            "Return {.cls ggplot} object", sprintf("of %s", id), "directly"
        ))
        rlang::return_from(sys.frame(1L), value = p)
    } else if (is.function(anno@debug)) {
        cli::cli_inform(sprintf("Debug from %s annotation", fn_id))
        anno@debug(p)
    }

    gt <- ggplot2::ggplotGrob(p) # nolint
    draw_fn <- function(index, k, n) {
        vp <- flip_viewport(which, xscale = c(0.5, n + 0.5), yscale = c(0, 1))
        if (with_slice) {
            if (which == "row") {
                pattern <- c("panel-%d-1")
                if (k == 1L) {
                    pattern <- c(pattern, "axis-t", "lab-t")
                } else if (k == n) {
                    pattern <- c(pattern, "axis-b", "lab-b")
                }
            } else {
                pattern <- "panel-1-%d"
                if (k == 1L) {
                    pattern <- c(pattern, "axis-l", "lab-l")
                } else if (k == n) {
                    pattern <- c(pattern, "axis-r", "lab-r")
                }
            }
            pattern <- paste0(sprintf(pattern, k), collapse = "|")
        } else {
            if (which == "row") {
                pattern <- c("panel", "axis-t", "axis-b", "lab-t", "lab-b")
            } else {
                pattern <- c("panel", "axis-l", "axis-r", "lab-l", "lab-r")
            }
            pattern <- paste0(pattern, collapse = "|")
        }
        vp_gt <- gt_trim_zero_grob(gtable::gtable_filter(gt, pattern))
        .ggfit_panel(vp_gt, vp = vp)
    }
    list(legend = legend_from_gtable(gt), draw_fn = draw_fn)
}
