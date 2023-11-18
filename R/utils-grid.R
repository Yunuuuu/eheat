# call grid function
# Always regard `y` as coord parallelly with heatmap and `y` as coord vertically
# with heatmap; x, x0, x1;
exec_grid <- function(fn, which, ...) {
    params <- list(...)
    if (which == "row") params <- flip_coord(params)
    do.call(fn, params)
}

exec_viewport <- function(which, ...) {
    exec_grid(grid::viewport, which = which, ...)
}

flip_coord <- function(aesthetics) {
    x <- c("x", "x0", "x1", "width", "xscale")
    y <- c("y", "y0", "y1", "height", "yscale")
    rename(aesthetics, structure(c(x, y), names = c(y, x)))
}

subset_gp <- function(gp, i) {
    params <- lapply(gp, `[[`, i)
    do.call(grid::gpar, params)
}

recycle_gp <- function(gp, n) {
    n <- max(n, 1L)
    for (i in seq_along(gp)) {
        gp[[i]] <- rep_len(gp[[i]], n)
    }
    gp
}
