#' @importFrom ComplexHeatmap ht_opt
#' @export
ComplexHeatmap::ht_opt

cheat_which <- function(which = NULL) {
    out <- cheat_env_get("current_annotation_which")
    if (is.null(out)) {
        out <- match.arg(which, c("column", "row"))
    }
    out
}

cheat_env_get <- function(name) {
    .subset2(cheat_env(), name)
}

cheat_env <- function() utils::getFromNamespace(".ENV", ns = "ComplexHeatmap")

# get slice informations from the draw function
cheat_get_order_list <- function(name, pos = 2L, return_env = FALSE) {
    trace_data(
        name = name,
        has_fn = function(env, name) {
            exists(name, envir = env, inherits = FALSE) &&
                methods::.hasSlot(.subset2(env, name), "row_order_list") &&
                methods::.hasSlot(.subset2(env, name), "column_order_list")
        },
        return_fn = function(env, name) {
            obj <- .subset2(env, name)
            list(
                row_order_list = obj@row_order_list,
                column_order_list = obj@column_order_list
            )
        },
        pos = pos, return_env = return_env
    )
}

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

guide_from_gtable <- function(gt, direction = NULL) {
    guides <- gtable::gtable_filter(gt, "guide-box")
    outs <- lapply(guides$grobs, function(x) {
        if (grid::is.grob(x) && inherits(x, "zeroGrob")) return(NULL) # styler: off
        guide <- gtable::gtable_filter(x, "guides")
        attr(guide, "width") <- sum(guide$widths)
        attr(guide, "height") <- sum(guide$heights)
        methods::new(
            "Legends",
            grob = guide,
            type = "gg_legend",
            name = "gg",
            n = 1L, multiple = 1L,
            # extract information directly from ggplot2 ? how to
            direction = match.arg(direction, c("vertical", "horizontal"))
        )
    })
    outs[!vapply(outs, is.null, logical(1L))]
}

guide_from_gg <- function(gg, direction = NULL) {
    guide_from_gtable(ggplot2::ggplotGrob(gg, direction = direction))
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
        labels <- lables[slice_data[[2L]]]
        do.call(scale_fn, list(
            limits = limits,
            breaks = breaks,
            labels = labels,
            expand = ggplot2::expansion()
        ))
    })
}
