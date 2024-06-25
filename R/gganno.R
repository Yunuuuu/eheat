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
#' @seealso [eanno]
#' @examples
#' draw(gganno(function(p) {
#'     p + geom_point(aes(y = V1))
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
#' @include ggheat.R
methods::setClass(
    "ggAnno",
    slots = list(ggparams = "list"),
    contains = "ExtendedAnnotation"
)

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
eheat_prepare.ggAnno <- function(anno, order_list, name) {
    if (is.null(name)) {
        id <- "(gganno)"
        fn_id <- "{.fn ggfn}"
    } else {
        id <- sprintf("(gganno: %s)", name)
        fn_id <- sprintf("{.fn ggfn} %s", id)
    }
    which <- anno@which
    # we always regard matrix row as the observations
    matrix <- anno@matrix
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
    p <- rlang::inject(anno@fun(p, !!!anno@dots))
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
    anno@fun <- function(index, k, n) {
        vp <- flip_viewport(which, xscale = c(0.5, n + 0.5), yscale = c(0, 1))
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
            elements = c("axis", "lab"),
            vp = vp
        )
    }
    anno@dots <- list()
    anno@legends_panel <- get_guides(gt, margins = "i")
    anno@legends_margin <- get_guides(gt)
    anno
}
