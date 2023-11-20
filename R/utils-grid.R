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

# https://stackoverflow.com/questions/29535760/fit-ggplot-exactly-to-viewport-size
fit_ggplot <- function(ggplot, vp, elements = c("b", "t", "l", "r")) {
    # Convert the plot to a grob
    gt <- ggplot2::ggplotGrob(ggplot)

    # Extract panel, axes and axis labels
    panel <- gtable::gtable_filter(gt, "panel")
    # panel <- egg::set_panel_size(
    #     ggplot, width = unit(1, "npc"), height = unit(1, "npc")
    # )
    grid::pushViewport(vp)
    grid::grid.draw(panel)

    if (any("l" == elements)) {
        # Put labels and axes together
        # Viewport for left axis and label
        lab_l <- gtable::gtable_filter(gt, "ylab-l")
        axis_l <- gtable::gtable_filter(gt, "axis-l")
        left <- cbind(lab_l, axis_l)

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

    if (any("r" == elements)) {
        # Viewport for right axis and label
        lab_r <- gtable::gtable_filter(gt, "ylab-r")
        axis_r <- gtable::gtable_filter(gt, "axis-r")
        right <- cbind(lab_r, axis_r)

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

    if (any("b" == elements)) {
        # Viewport for bottom axis and label
        axis_b <- gtable::gtable_filter(gt, "axis-b")
        lab_b <- gtable::gtable_filter(gt, "xlab-b")
        bottom <- rbind(axis_b, lab_b)
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

    if (any("t" == elements)) {
        # Viewport for top axis and label
        axis_t <- gtable::gtable_filter(gt, "axis-t")
        lab_t <- gtable::gtable_filter(gt, "xlab-t")
        top <- rbind(axis_t, lab_t)
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
    grid::popViewport()
}
