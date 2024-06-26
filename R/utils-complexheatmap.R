#' Creates a gTree object from the Heatmap
#' @param x A [HeatmapList-class][ComplexHeatmap::HeatmapList-class] object.
#' @inheritDotParams grid::grid.grabExpr -expr
#' @return A [gTree][grid::gTree] object.
#' @examples
#' h <- ggheat(matrix(rnorm(100), 10))
#' eheat_grob(h)
#' @export
eheat_grob <- function(x, ...) grid::grid.grabExpr(expr = draw(x), ...)

#' Decorate ComplexHeatmap
#' @param vp_name A valid viewport name returned by
#' [list_components][ComplexHeatmap::list_components]
#' @param code Codes to draw elements in the viewport.
#' @return Decorate the viewport.
#' @examples
#' ggheat(matrix(rnorm(81), nrow = 9), column_km = 2L, name = "eheat_decorate")
#' ComplexHeatmap::list_components()
#' eheat_decorate("eheat_decorate_heatmap_body_wrap", {
#'     grid.text("I'm from eheat_decorate",
#'         1.5 / 10, 2.5 / 4,
#'         default.units = "npc"
#'     )
#' })
#' @export
eheat_decorate <- function(vp_name, code) {
    assert_string(vp_name, empty_ok = FALSE)
    components <- ComplexHeatmap::list_components()
    if (!length(components)) {
        cli::cli_abort(c(
            "No valid ComplexHeatmap components",
            i = "You must draw heatmap/annotation components first"
        ))
    }
    if (!any(vp_name == components)) {
        cli::cli_abort(c(
            "Cannot find {.field {vp_name}}",
            i = paste(
                "check components with",
                "{.code ComplexHeatmap::list_components()}"
            )
        ))
    }
    eval(substitute(.eheat_decorate(vp_name, code)))
}

.eheat_decorate <- function(vp_name, code) {
    current_vp <- grid::current.viewport()$name
    if (current_vp == "ROOT") current_vp <- "global"
    grid::seekViewport(vp_name)
    on.exit(grid::seekViewport(current_vp))
    force(code)
}

#' Temporarily change ComplexHeatmap options.
#'
#' @param opts New [ht_opt][ComplexHeatmap::ht_opt], must be a named list.
#' @inheritParams with_ht_verbose
#' @inherit with_ht_verbose return
#' @note ComplexHeatmap heatmap options will always be reset with
#' `ComplexHeatmap::ht_opt(RESET = TRUE)` when function exit.
#' @seealso [ht_opt][ComplexHeatmap::ht_opt]
#' @examples
#' with_ht_opts(
#'     list(verbose = TRUE),
#'     ggheat(matrix(rnorm(81), nrow = 9))
#' )
#' @importFrom ComplexHeatmap ht_opt
#' @export
with_ht_opts <- function(opts, code) {
    assert_(opts, function(x) is.list(x) && rlang::is_named(x), "a named list")
    # nms <- rlang::names2(opts)
    # old <- rlang::inject(ht_opt(!!!nms))
    # if (!is.list(old)) old <- list(old)
    # names(old) <- nms
    ht_opt(opts) # set options
    # if (length(old)) on.exit(ht_opt(old)) # reset option
    on.exit(ht_opt(RESET = TRUE)) # reset option
    force(code)
}

#' Make ComplexHeatmap verbose
#' @param code Running [ggheat] or [Heatmap][ComplexHeatmap::Heatmap] function.
#' @param verbose A boolean value indicates whether to draw Heatmap verbosely.
#' @return The results of the evaluation of the `code` argument.
#' @examples
#' with_ht_verbose(ggheat(matrix(rnorm(81), nrow = 9)))
#' @seealso [with_ht_opts]
#' @export
with_ht_verbose <- function(code, verbose = TRUE) {
    old <- ht_opt("verbose")
    ht_opt(verbose = verbose)
    on.exit(ht_opt(verbose = old))
    force(code)
}

set_ht_opt <- function(opt, value) {
    old <- ht_opt[[opt]]
    ht_opt[[opt]] <- value
    invisible(old)
}

eheat_which <- function(which = NULL) {
    out <- eheat_env_get("current_annotation_which")
    if (is.null(out)) {
        out <- match.arg(which, c("column", "row"))
    }
    out
}

eheat_env_get <- function(name) .subset2(eheat_env(), name)

eheat_env <- function() utils::getFromNamespace(".ENV", ns = "ComplexHeatmap")

eheat_full_slice_index <- function(order_list) {
    row_full <- unlist(order_list$row, recursive = FALSE, use.names = FALSE)
    row_full <- structure(seq_along(row_full), names = row_full)
    column_full <- unlist(order_list$column,
        recursive = FALSE, use.names = FALSE
    )
    column_full <- structure(seq_along(column_full), names = column_full)
    out <- vector("list")
    for (i in seq_along(order_list$row)) {
        row_order <- order_list$row[[i]]
        nr <- length(row_order)
        for (j in seq_along(order_list$column)) {
            column_order <- order_list$column[[j]]
            nc <- length(column_order)
            expand_idx <- expand.grid(seq_len(nr), seq_len(nc))
            out[[sprintf("r%dc%d", i, j)]] <- data_frame0(
                .slice_row = i,
                .slice_column = j,
                .row_index = row_order[expand_idx[[1L]]],
                .column_index = column_order[expand_idx[[2L]]],
                x = column_full[as.character(.data$.column_index)],
                y = row_full[as.character(.data$.row_index)]
            )
        }
    }
    out
}

eheat_text_just <- function(rot, side) {
    rot <- rot %% 180
    if (side == "left") {
        if (rot == 0) {
            return(c(1, 0.5))
        } else if (rot == 90) {
            return(c(0.5, 0))
        } else if (rot == 270) {
            return(c(0.5, 1))
        }
    } else if (side == "right") {
        if (rot >= 0 && rot < 90) {
            return(c(0, 0.5))
        } else if (rot == 90) {
            return(c(0.5, 1))
        } else if (rot > 90 && rot < 180) {
            return(c(0, 0.5))
        }
    } else if (side == "top") {
        if (rot == 0) {
            return(c(0.5, 0))
        } else if (rot > 0 && rot <= 90) {
            return(c(0, 0.5))
        } else if (rot > 90 && rot <= 180) {
            return(c(1, 0.5))
        }
    } else if (side == "bottom") {
        if (rot == 0) {
            return(c(0.5, 1))
        } else if (rot > 0 && rot <= 90) {
            return(c(1, 0.5))
        } else if (rot > 90 && rot <= 180) {
            return(c(0, 0.5))
        }
    }
}

eheat_check_gp <- function(gp) {
    if (!"lineheight" %in% names(gp)) {
        gp$lineheight <- 0.9
    }
    if (!inherits(gp, "gpar")) {
        cli::cli_abort("Graphic parameters should be specified by `gpar()`.")
    }
    gp
}

#' @param data A data.frame in the order of slice, coord, index
#' @noRd
eheat_scales <- function(data, lables, scale_fn) {
    lapply(split(data, data[[1L]]), function(slice_data) {
        slice_data <- slice_data[2:3]
        slice_data <- unique(slice_data)
        slice_data <- slice_data[order(slice_data[[1L]]), ]
        breaks <- slice_data[[1L]]
        limits <- range(breaks)
        limits[1L] <- limits[1L] - 0.5
        limits[2L] <- limits[2L] + 0.5
        if (!inherits(labels, "waiver")) {
            labels <- lables[slice_data[[2L]]]
        }
        do.call(scale_fn, list(
            limits = limits,
            breaks = breaks,
            labels = labels,
            expand = ggplot2::expansion()
        ))
    })
}

wrap_legend <- function(legend) {
    if (length(legend) > 0L && !is.list(legend)) {
        list(legend)
    } else {
        legend
    }
}

is_from_eheat <- function(env) {
    identical(utils::packageName(topenv(env)), "ComplexHeatmap")
}
