#' Creates a gTree object from the Heatmap
#' @param x A [HeatmapList-class][ComplexHeatmap::HeatmapList-class] object.
#' @inheritDotParams grid::grid.grabExpr -expr
#' @return A [gTree][grid::gTree] object.
#' @examples
#' h <- ggheat(matrix(rnorm(100), 10))
#' cheat_grob(h)
#' @export
cheat_grob <- function(x, ...) grid::grid.grabExpr(expr = draw(x), ...)

cheat_decorate <- function(vp_name, code) {
    # ComplexHeatmap::list_components
    current_vp <- grid::current.viewport()$name
    on.exit(grid::seekViewport(current_vp))
    if (current_vp == "ROOT") current_vp <- "global"
    grid::seekViewport(vp_name)
    force(code)
}

cheat_which <- function(which = NULL) {
    out <- cheat_env_get("current_annotation_which")
    if (is.null(out)) {
        out <- match.arg(which, c("column", "row"))
    }
    out
}

cheat_env_get <- function(name) .subset2(cheat_env(), name)

cheat_env <- function() utils::getFromNamespace(".ENV", ns = "ComplexHeatmap")

cheat_full_slice_index <- function(order_list) {
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
                .row = row_full[as.character(.data$.row_index)],
                .column = column_full[as.character(.data$.column_index)]
            )
        }
    }
    out
}

cheat_text_just <- function(rot, side) {
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

cheat_check_gp <- function(gp) {
    if (!"lineheight" %in% names(gp)) {
        gp$lineheight <- 0.9
    }
    if (!inherits(gp, "gpar")) {
        cli::cli_abort("Graphic parameters should be specified by `gpar()`.")
    }
    gp
}

# column order in slice, coord, raw_index
cheat_scales <- function(data, lables, scale_fn) {
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
    if (length(legend) > 0L && inherits(legend, c("Legends", "grob"))) {
        list(legend)
    } else {
        legend
    }
}

# here is the magic
#' @param name "heatmap_legend_list" or "annotation_legend_list"
#' @param gglegends By calling `make_legends` function.
#' @noRd
add_gg_legend_list <- function(name, gglegends, call = quote(make_layout)) {
    if (length(gglegends) == 0L) return(NULL) # styler: off
    pos <- -2L
    nframes <- -sys.nframe() + 1L # total parents
    while (pos >= nframes) {
        env <- sys.frame(pos) # we locate the legend environment
        if (identical(utils::packageName(topenv(env)), "ComplexHeatmap") &&
            exists(name, envir = env, inherits = FALSE) &&
            # Since ComplexHeatmap function much are the S4 methods
            # we identify the call name from the parent generic function
            identical(sys.call(pos - 1L)[[1L]], call)) {
            old <- wrap_legend(.subset2(env, name))
            index <- grep("^\\.gg_legend\\d+$", rlang::names2(old), perl = TRUE)
            old_gglegends <- old[index]
            names(gglegends) <- paste0(
                ".__gg_legend", seq_along(gglegends) + length(old_gglegends)
            )
            # we then modify the legend list
            assign(
                # user provided legends always in the end
                name, c(old_gglegends, gglegends, old[-index]),
                envir = env
            )
            break
        }
        pos <- pos - 1L
    }
}
