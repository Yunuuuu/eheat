# call grid function
# Always regard `y` as coord parallelly with heatmap and `y` as coord vertically
# with heatmap; x, x0, x1;
flip_grid <- function(fn, which, ...) {
    params <- rlang::list2(...)
    if (which == "row") params <- flip_gp(params)
    do.call(fn, params)
}

flip_viewport <- function(which, ...) {
    flip_grid(grid::viewport, which = which, ...)
}

flip_gp <- function(gp) {
    x <- c("x", "x0", "x1", "width", "xscale")
    y <- c("y", "y0", "y1", "height", "yscale")
    rename(gp, structure(c(x, y), names = c(y, x)))
}

subset_gp <- function(gp, i) {
    params <- lapply(gp, `[[`, i)
    do.call(gpar, params)
}

recycle_gp <- function(gp, n) {
    n <- max(n, 1L)
    for (i in seq_along(gp)) {
        gp[[i]] <- rep_len(gp[[i]], n)
    }
    gp
}

grid_vp_size <- function() {
    current_vp <- grid::current.viewport()$name
    if (current_vp == "ROOT") {
        grid::unit(graphics::par("din"), "in")
    } else {
        grid::upViewport()
        on.exit(grid::downViewport(current_vp))
        grid::unit.c(
            grid::convertWidth(grid::unit(1, "npc"), "mm"),
            grid::convertHeight(grid::unit(1, "npc"), "mm")
        )
    }
}

# https://stackoverflow.com/questions/29535760/fit-ggplot-exactly-to-viewport-size
fit_panel <- function(gt, vp, elements = c("b", "t", "l", "r")) {
    # Convert the plot to a grob
    # gt <- ggplot2::ggplotGrob(ggplot)

    # Extract panel, axes and axis labels
    panel <- gtable::gtable_filter(gt, "panel")
    # panel <- egg::set_panel_size(
    #     ggplot, width = unit(1, "npc"), height = unit(1, "npc")
    # )
    grid::pushViewport(vp)
    grid::grid.draw(panel)
    grid::popViewport()

    if (any("l" == elements)) {
        # Put labels and axes together
        # Viewport for left axis and label
        lab_l <- gtable::gtable_filter(gt, "ylab-l")
        axis_l <- gtable::gtable_filter(gt, "axis-l")
        left <- gt_bind(cbind, lab_l, axis_l)

        if (length(left)) {
            # Get their width
            w <- grid::convertX(sum(left$widths), "mm")
            vp_left <- grid::viewport(
                x = unit(0, "npc") - .5 * w,
                y = unit(0.5, "npc"),
                width = w,
                height = unit(1, "npc")
            )
            grid::pushViewport(vp_left)
            grid::grid.draw(left)
            grid::popViewport()
        }
    }

    if (any("r" == elements)) {
        # Viewport for right axis and label
        lab_r <- gtable::gtable_filter(gt, "ylab-r")
        axis_r <- gtable::gtable_filter(gt, "axis-r")
        right <- gt_bind(cbind, axis_r, lab_r)
        if (length(right)) {
            # Get their width
            w <- grid::convertX(sum(right$widths), "mm")
            vp_right <- grid::viewport(
                x = unit(1, "npc") + .5 * w,
                y = unit(0.5, "npc"),
                width = w,
                height = unit(1, "npc")
            )
            grid::pushViewport(vp_right)
            grid::grid.draw(right)
            grid::popViewport()
        }
    }

    if (any("b" == elements)) {
        # Viewport for bottom axis and label
        axis_b <- gtable::gtable_filter(gt, "axis-b")
        lab_b <- gtable::gtable_filter(gt, "xlab-b")
        bottom <- gt_bind(rbind, axis_b, lab_b)
        if (length(bottom)) {
            # Get their width / height
            h <- grid::convertX(sum(bottom$heights), "mm")
            vp_bottom <- grid::viewport(
                x = unit(0.5, "npc"),
                y = unit(0, "npc") - .5 * h,
                width = unit(1, "npc"), height = h
            )
            grid::pushViewport(vp_bottom)
            grid::grid.draw(bottom)
            grid::popViewport()
        }
    }

    if (any("t" == elements)) {
        # Viewport for top axis and label
        axis_t <- gtable::gtable_filter(gt, "axis-t")
        lab_t <- gtable::gtable_filter(gt, "xlab-t")
        top <- gt_bind(rbind, lab_t, axis_t)
        if (length(top)) {
            # Get their width / height
            h <- grid::convertX(sum(top$heights), "mm")
            vp_top <- grid::viewport(
                x = unit(0.5, "npc"),
                y = unit(1, "npc") + .5 * h,
                width = unit(1, "npc"), height = h
            )
            grid::pushViewport(vp_top)
            grid::grid.draw(top)
            grid::popViewport()
        }
    }
}

gt_bind <- function(fn, ...) {
    dots <- list(...)
    dots <- dots[lengths(dots) > 0L]
    if (length(dots) > 1) {
        do.call(fn, dots)
    } else if (length(dots) == 1L) {
        dots[[1L]]
    } else {
        gtable::gtable()
    }
}

gt_unify_panel <- function(gt, width, height) {
    panels <- grep("panel", gt$layout$name)
    gt_set_size(gt, width, height, panels)
}

gt_set_size <- function(gt, width, height, index = NULL) {
    index_w <- unique(gt$layout$l[index])
    index_h <- unique(gt$layout$t[index])
    nw <- length(index_w)
    nh <- length(index_h)
    gt$widths[index_w] <- rep(width, nw)
    gt$heights[index_h] <- rep(height, nh)
    gt
}

is_zero_grob <- function(x) inherits(x, "zeroGrob")

gt_trim_zero_grob <- function(x) {
    matches <- !vapply(x$grobs, is_zero_grob, logical(1L))
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
}

# gtable_split_by_panel <- function(gt) {
#     gt <- gt_subset(
#         gt, !grepl("background", .subset2(gt$layout, "name"))
#     )
#     matches <- grepl("panel", .subset2(gt$layout, "name"))
#     panels <- gt_subset(gt, matches)
#     if (length(panels) == 0L) {
#         cli::cli_abort("No {.field panels} found")
#     } else if (length(panels) == 1L) {
#         return(gt)
#     }
#     p_layout <- panels$layout
#     p_nms <- .subset2(p_layout, "name")
#     others <- gt_subset(gt, !matches)
#     o_layout <- others$layout
#     lapply(p_nms, function(nm) {
#         current_p_layout <- p_layout[p_layout$name == nm, , drop = FALSE]
#         other_p_layout <- p_layout[
#             p_layout$name %in% setdiff(p_nms, nm), ,
#             drop = FALSE
#         ]
#         #
#         if (all(current_p_layout$t <= other_p_layout$t)) {
#             index <- o_layout$b >= current_p_layout$t
#         } else {

#         }
#     })
# }

gt_subset <- function(gt, i) {
    gt$layout <- gt$layout[i, , drop = FALSE]
    gt$grobs <- gt$grobs[i]
    gt
}
