#' @examples
#' gpar(col = "red")
#' @importFrom grid gpar
#' @return 
#' - `gpar`: An object of class [gpar][grid::gpar].
#' @export
grid::gpar

#' @examples
#' unit(1, "npc")
#' @importFrom grid unit
#' @return 
#' - `unit`: An object of class [unit][grid::unit].
#' @export
grid::unit

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
    do.call(grid::gpar, lapply(gp, .subset2, i))
}

recycle_gp <- function(gp, n) {
    n <- max(n, 1L)
    for (i in seq_along(gp)) {
        gp[[i]] <- rep_len(gp[[i]], n)
    }
    gp
}

with_viewport <- function(vp, code) {
    grid::pushViewport(vp)
    on.exit(grid::popViewport())
    force(code)
}

grid_draw <- function(vp, grob) {
    with_viewport(vp, grid::grid.draw(grob))
}

is_zero_grob <- function(x) inherits(x, "zeroGrob")

# Get viewport sizes ---------------------
vp_size <- function() {
    if (grid::current.viewport()$name == "ROOT") {
        grid::unit(graphics::par("din"), "in")
    } else {
        grid::unit.c(
            grid::convertWidth(grid::unit(1, "npc"), "mm"),
            grid::convertHeight(grid::unit(1, "npc"), "mm")
        )
    }
}
