#' Build ggAnno Class
#'
#' @details
#' `gganno` can be seamlessly combined with both
#' [Heatmap][ComplexHeatmap::Heatmap] and [ggheat], although legends will not be
#' extracted in the later case.
#'
#' @inheritParams eanno
#' @param ... Additional arguments passed to `ggfn`.
#' @inheritParams ComplexHeatmap::AnnotationFunction
#' @section ggfn:
#'
#' `ggfn` accept a ggplot2 object with a default data and mapping created by
#' `ggplot(data, aes(.data$x))` / `ggplot(data, ggplot2::aes(y = .data$y))`.
#' The original matrix will be converted into a long-data.frame (`gganno` always
#' regard row as the observations) with following columns:
#' - `.slice`: the slice row (which = `"row"`) or column (which = `"column"`)
#'   number.
#' - `.row_names` and `.column_names`: the row and column names of the original
#'   matrix (only applicable when names exist).
#' - `.row_index` and `.column_index`: the row and column index of the original
#'   matrix.
#' - `x` / `y`: indicating the x-axis (or y-axis) coordinates. Don't use
#'   [coord_flip][ggplot2::coord_flip] to flip coordinates as it may disrupt
#'   internal operations.
#' - `value`: the actual matrix value of the annotation matrix.
#'
#' @inherit ggheat
#' @seealso [eanno]
#' @examples
#' draw(gganno(function(p) {
#'     p + geom_point(aes(y = value))
#' }, matrix = rnorm(10L), height = unit(10, "cm"), width = unit(0.7, "npc")))
#' @return A `ggAnno` object.
#' @export
#' @name gganno
gganno <- function(ggfn, ..., matrix = NULL,
                   which = NULL, width = NULL, height = NULL) {
    out <- eanno(
        draw_fn = ggfn, ..., matrix = matrix,
        which = which, width = width, height = height,
        show_name = FALSE, fun_name = "gganno"
    )
    out <- methods::as(out, "ggAnno")
    out
}

#' @export
#' @rdname gganno
#' @include eanno.R
methods::setClass("ggAnno", contains = "ExtendedAnnotation")

#' @inheritParams internal-method
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @export
#' @rdname eheat_prepare
eheat_prepare.ggAnno <- function(object, ..., viewport, heatmap, name) {
    if (is.null(name)) {
        id <- "(gganno)"
        fn_id <- "{.fn ggfn}"
    } else {
        id <- sprintf("(gganno: %s)", name)
        fn_id <- sprintf("{.fn ggfn} %s", id)
    }
    which <- object@which
    # we always regard matrix row as the observations
    matrix <- object@matrix
    if (is.null(heatmap)) {
        order_list <- list(seq_len(nrow(matrix)))
    } else {
        order_list <- switch(which,
            row = heatmap@row_order_list,
            column = heatmap@column_order_list
        )
    }
    if (length(order_list) > 1L) {
        with_slice <- TRUE
    } else {
        with_slice <- FALSE
    }
    row_nms <- rownames(matrix)
    col_nms <- colnames(matrix)
    data <- as_tibble0(matrix, rownames = NULL) # nolint
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

    coords <- data_frame0(
        .slice = rep(
            seq_along(order_list),
            times = lengths(order_list)
        ),
        .row_index = unlist(order_list, recursive = FALSE, use.names = FALSE),
        x = seq_along(.data$.row_index)
    )
    data <- merge(coords, data, by = ".row_index", all = FALSE)
    nms <- c(
        ".slice", ".row_names", ".column_names",
        ".row_index", ".column_index", "x", "y", "value"
    )
    if (which == "row") {
        data <- rename(data, c(x = "y"))
        if (with_slice) {
            data <- lapply(split(data, data$.slice), function(subdata) {
                subdata$y <- reverse_trans(subdata$y)
                subdata
            })
            data <- do.call(rbind, data)
            data <- as_tibble0(data, rownames = NULL)
        } else {
            data$y <- reverse_trans(data$y)
        }
        p <- ggplot(data[intersect(nms, names(data))], aes(y = .data$y))
    } else {
        p <- ggplot(data[intersect(nms, names(data))], aes(x = .data$x))
    }
    p <- rlang::inject(object@fun(p, !!!object@dots))
    object@dots <- list() # remove dots
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
    labels <- row_nms %||% ggplot2::waiver()
    if (which == "row") {
        facet_params <- list(
            rows = ggplot2::vars(.data$.slice),
            scales = "free_y", space = "free_y"
        )
        y_scale <- eheat_scales(
            coords[c(1L, 3:2)], labels,
            scale_fn = ggplot2::scale_y_continuous
        )
        if (!is.null(p$scales$get_scales("y"))) {
            cli::cli_warn(paste("will omit y-scale for row annotation", id))
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
        x_scale <- eheat_scales(coords[c(1L, 3:2)], labels,
            scale_fn = ggplot2::scale_x_continuous
        )
        if (!is.null(p$scales$get_scales("x"))) {
            cli::cli_warn(paste("will omit x-scale for column annotation", id))
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

    gt <- ggplot2::ggplotGrob(p) # nolint
    object@fun <- function(index, k, n) {
        if (with_slice) {
            m <- NULL
            if (which == "row") {
                pattern <- c("panel-%d-1")
                if (k == 1L) {
                    m <- "t"
                } else if (k == n) {
                    m <- "b"
                }
            } else {
                pattern <- "panel-1-%d"
                if (k == 1L) {
                    m <- "l"
                } else if (k == n) {
                    m <- "r"
                }
            }
            pattern <- sprintf(pattern, k)
        } else {
            pattern <- "panel"
            if (which == "row") {
                m <- c("t", "b")
            } else {
                m <- c("l", "r")
            }
        }
        .ggfit(
            gt_area(gt, pattern, margins = m),
            align_with = "panel", margins = m,
            elements = c("axis", "lab")
        )
    }
    object@legends_panel <- get_guides(gt, margins = "i")
    object@legends_margin <- get_guides(gt)
    object
}
