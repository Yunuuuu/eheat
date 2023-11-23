#' Build AnnotationFunction Class with ggplot2
#'
#' @details
#' Both `gganno` and `gganno2` perform identical functions, but `gganno` is not
#' compatible with direct integration with [Heatmap][ComplexHeatmap::Heatmap].
#' In such cases, only an empty annotation region can be added. On the other
#' hand, `gganno2` can be seamlessly combined with both
#' [Heatmap][ComplexHeatmap::Heatmap] and [ggheat], although legends will not be
#' extracted.
#'
#' @inheritParams ggheat
#' @param ... Other arguments passed to `ggfn`.
#' @inheritParams ComplexHeatmap::AnnotationFunction
#' @return A `ggAnnotationFunction` object.
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
#' @export
#' @name gganno
gganno <- function(matrix, ggfn, ..., which = NULL, width = NULL, height = NULL, debug = FALSE) {
    matrix <- build_matrix(matrix)
    ggfn <- allow_lambda(ggfn)
    debug <- allow_lambda(debug)
    ggparams <- rlang::list2(...)
    out <- new_anno(
        n = nrow(matrix),
        draw_fn = function(index, k, n) {
            return(NULL)
        },
        ylim = NULL, subset_rule = list(), subsettable = FALSE,
        which = which, width = width, height = height,
        show_name = FALSE, name = "gganno"
    )
    out <- methods::as(out, "ggAnnotationFunction")
    out@matrix <- matrix
    out@ggfn <- ggfn
    out@ggparams <- ggparams
    out@debug <- debug
    out
}

#' @export
#' @rdname gganno
gganno2 <- function(
    matrix, ggfn, ..., which = NULL,
    width = NULL, height = NULL, debug = FALSE) {
    anno <- gganno(matrix,
        ggfn = ggfn, ..., which = which,
        width = width, height = height,
        debug = debug
    )
    ddraw <- NULL
    draw_fn <- function(index, k, n) {
        if (k == 1L) {
            # only prepare ggplot data in the first run and run everytime when
            # draw function execution
            # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/HeatmapList-draw_component.R
            # trace back into `draw_heatmap_list()`
            order_list <- cheat_get_order_list("ht_main")
            order_list <- switch(which,
                row = order_list$row_order_list,
                column = order_list$column_order_list
            )
            ddraw <<- draw_gganno(anno, NULL, order_list, which = which)$draw_fn
        }
        ddraw(index, k, n)
    }
    new_anno(
        n = nrow(anno@matrix), draw_fn = draw_fn, ylim = NULL,
        subset_rule = list(), subsettable = FALSE,
        which = which, width = width, height = height,
        show_name = FALSE, name = "gganno2"
    )
}

methods::setClassUnion("MatrixOrNull", c("matrix", "NULL"))

#' @importClassesFrom ComplexHeatmap AnnotationFunction
#' @export
#' @rdname gganno
#' @include ggheat.R
methods::setClass(
    "ggAnnotationFunction",
    slots = list(
        ggfn = "FunctionOrNull",
        ggparams = "list",
        matrix = "MatrixOrNull",
        debug = "ANY"
    ),
    contains = "AnnotationFunction"
)

draw_gganno <- function(anno, heat_matrix, order_list, which, id) {
    matrix <- anno@matrix %||% switch(which,
        row = heat_matrix,
        column = t(heat_matrix)
    )
    row_nms <- rownames(matrix)
    data <- tibble::as_tibble(matrix, .name_repair = "minimal") # nolint
    if (length(order_list) > 1L) {
        with_slice <- TRUE
    } else {
        with_slice <- FALSE
    }
    coord <- data_frame0(
        .slice = rep(
            seq_along(order_list),
            times = lengths(order_list)
        ),
        .index = unlist(order_list, recursive = FALSE, use.names = FALSE),
        .x = seq_along(.data$.index)
    )
    data <- cbind(coord, data[match(coord$.index, seq_len(nrow(data))), ])
    if (which == "row") {
        orientation <- ".y"
        data <- rename(data, c(.x = ".y"))
        if (with_slice) {
            data <- lapply(split(data, data$.slice), function(subdata) {
                subdata$.y <- reverse_trans(subdata$.y)
                subdata
            })
            data <- do.call(rbind, data)
            data <- tibble::as_tibble(data, .name_repair = "minimal")
        } else {
            data$.y <- reverse_trans(data$.y)
        }
        p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$.y))
    } else {
        orientation <- ".x"
        p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$.x))
    }
    p <- rlang::inject(anno@ggfn(p, !!!anno@ggparams))
    if (!ggplot2::is.ggplot(p)) {
        cli::cli_abort(
            "{.arg ggfn} of {id} must return a {.cls ggplot2} object."
        )
    }
    if (which == "row") {
        facet_params <- list(
            rows = ggplot2::vars(.data$.slice),
            scales = "free_y", space = "free_y"
        )
        scale_fn <- ggplot2::scale_y_continuous
    } else {
        facet_params <- list(
            cols = ggplot2::vars(.data$.slice),
            scales = "free_x", space = "free_x"
        )
        scale_fn <- ggplot2::scale_x_continuous
    }
    if (with_slice) {
        p <- p + do.call(ggplot2::facet_grid, facet_params)
        # ".slice" and ".x"/".y"
        scales <- lapply(split(data, data$.slice), function(subdata) {
            limits <- range(subdata[[orientation]])
            labels <- row_nms[subdata$.index][order(subdata[[orientation]])]
            labels <- unique(labels)
            do.call(scale_fn, list(
                limits = c(limits[1L] - 0.5, limits[2L] + 0.5),
                breaks = sort(unique(subdata[[orientation]])),
                labels = labels,
                expand = ggplot2::expansion()
            ))
        })
        if (which == "row") {
            p <- p + ggh4x::facetted_pos_scales(
                x = p$scales$get_scales("x"),
                y = scales
            )
        } else {
            p <- p + ggh4x::facetted_pos_scales(
                x = scales, 
                y = p$scales$get_scales("y")
            )
        }
    } else {
        limits <- range(data[[orientation]])
        labels <- row_nms[data$.index][order(data[[orientation]])]
        labels <- unique(labels)
        p <- p + do.call(scale_fn, list(
            limits = c(limits[1L] - 0.5, limits[2L] + 0.5),
            breaks = coord$.x, labels = labels,
            expand = ggplot2::expansion()
        ))
    }
    if (isTRUE(anno@debug)) {
        cli::cli_inform(
            "Return {.cls ggplot} object from {.fn ggfn} of {id} annotation"
        )
        rlang::return_from(sys.frame(which = 1L), value = p)
    } else if (is.function(anno@debug)) {
        cli::cli_inform("Debug from {.fn ggfn} of {id} annotation")
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
            fit_panel(
                trim_zero_grob(gtable::gtable_filter(gt, pattern)),
                vp = vp
            )
        } else {
            fit_panel(trim_zero_grob(gt), vp = vp, elements = NULL)
        }
    }
    list(legend = guide_from_gtable(gt), draw_fn = draw_fn)
}
