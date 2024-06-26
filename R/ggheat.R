#' Extended Heatmap
#'
#' Plot heatmaps layer by layer
#'
#' @inheritParams eheat
#' @param ggfn A function or formula, accept a initial [ggplot][ggplot2::ggplot]
#' data as the input and must return a [ggplot][ggplot2::ggplot] object. For the
#' passed data details, please see section `ggfn`.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function with up to
#'   two arguments: `.x` (single argument) or `.x` and `.y` (two arguments). The
#'   `.` placeholder can be used instead of `.x`.  This allows you to create
#'   very compact anonymous functions (lambdas) with up to two inputs.
#'
#' @param ... Additional arguments passed on to [eheat].
#' @param ggparams Other arguments passed to `ggfn`.
#' @section ggfn:
#'
#' `ggfn` accept a ggplot2 object with a default data and mapping created by
#' `ggplot(data, aes(.data$x, .data$y))`.  the data contains following columns:
#' - `.slice`: slice number, combine `.slice_row` and `.slice_column`.
#' - `.slice_row`: the slice row number
#' - `.slice_column`: the slice column number
#' - `.row_names` and `.column_names`: the row and column names of the original
#'   matrix (only applicable when names exist).
#' - `.row_index` and `.column_index`: the row and column index of the original
#'   matrix.
#' - `x` and `y`: the `x` and `y` coordinates
#' - `value`: the actual matrix value for the heatmap matrix.
#'
#' @note Maintaining the internal limits along the heatmap to align well with
#' `ComplexHeatmap` is important.
#' @return A `ggHeatmap` Object.
#' @examples
#' ggheat(matrix(rnorm(81), nrow = 9))
#' @export
ggheat <- function(matrix, ggfn = NULL, ..., ggparams = list()) {
    out <- eheat(matrix = matrix, ...)
    ggfn <- allow_lambda(ggfn)
    out <- methods::as(out, "ggHeatmap")
    out@ggfn <- ggfn
    out@ggparams <- ggparams
    out
}

#' @export
#' @rdname ggheat
methods::setClass(
    "ggHeatmap",
    slots = list(ggfn = "ANY", ggparams = "list"),
    contains = "ExtendedHeatmap"
)

methods::setValidity("ggHeatmap", function(object) {
    ggfn <- object@ggfn
    if (!is.null(ggfn) && !is.function(ggfn)) {
        cli::cli_abort("{.code @ggfn} must be a function or NULL")
    }
    TRUE
})

#' @export
#' @rdname eheat_prepare
eheat_prepare.ggHeatmap <- function(object, ...) {
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
        values_to = "value"
    )
    data$.column_index <- as.integer(data$.column_index)
    if (!is.null(row_nms)) data$.row_names <- row_nms[data$.row_index]
    if (!is.null(col_nms)) data$.column_names <- col_nms[data$.column_index]

    # prepare slice panels data ------------------------
    slice_list <- eheat_full_slice_index(order_list)

    # prepare data for ggplot2 --------------------------
    coords <- lapply(slice_list, function(data) {
        data$.slice <- sprintf("r%dc%d", data$.slice_row, data$.slice_column)
        # reverse y-axis as ggplot2 and ComplexHeatmap draw in different
        # direction, but we cannot draw anything if we use `scale_y_reverse`, I
        # don't know why?. So we just reverse the values
        data$y <- reverse_trans(data$y)
        data
    })
    coords <- do.call(rbind, coords)
    data <- merge(coords, data,
        by = c(".row_index", ".column_index"),
        all = FALSE
    )

    # create the ggplot2 object --------------------
    nms <- c(
        ".slice", ".slice_row", ".slice_column",
        ".row_names", ".column_names",
        ".row_index", ".column_index",
        "x", "y", "value"
    )
    p <- ggplot(data[intersect(nms, names(data))], aes(.data$x, .data$y))

    # special case for rect_gp$type == "none" ------
    if (!identical(rect_gp$type, "none")) {
        # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
        # ggplot2::labs has relative low priorities, so user can provide scale
        #          name to overwrite the name
        p <- p + ggplot2::geom_tile(
            aes(.data$x, .data$y, fill = .data$value),
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
            cols <- c(".slice_row", "y", ".row_index")
        } else {
            fn <- ggplot2::scale_x_continuous
            labels <- col_nms
            cols <- c(".slice_column", "x", ".column_index")
        }
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
    # this function will be called by `draw_heatmap_body`
    # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/Heatmap-draw_component.R
    # combine `layer_fun` with `ggfn`, in this ways, the ComplexHeatmap
    # run layer_fun will call ggfun
    layer_fun_call_ggfn <- function(j, i, x, y, w, h, fill) {
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
    }
    object@matrix_param$layer_fun <- layer_fun_call_ggfn

    object@legends_panel <- get_guides(gt, margins = "i")
    object@legends_margin <- get_guides(gt)

    # we always prevent the ComplexHeatmap Heatmap body legend.
    object@heatmap_param$show_heatmap_legend <- FALSE
    object
}
