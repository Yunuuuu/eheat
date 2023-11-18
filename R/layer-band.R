layer_band <- function(
    ..., band_col = NA,
    band_fill = c("white", "#eff3f2"),
    band_alpha = NA, band_linewidth = 0.5,
    band_linetype = 1L, direction = "row",
    lineend = "butt", linejoin = "mitre",
    matrix = NULL) {
    new_layer(
        geom = eheatBandGeom, ...,
        direction = direction, band_col = band_col,
        band_fill = band_fill,
        band_alpha = band_alpha,
        band_linewidth = band_linewidth,
        band_linetype = band_linetype,
        lineend = lineend, linejoin = linejoin,
        matrix = matrix,
        name = "eheat_rect"
    )
}

eheatBandGeom <- ggplot2::ggproto("eheatBandGeom", eheatGeom,
    required_aes = character(),
    non_missing_aes = character(),
    optional_aes = character(),
    default_aes = list(),
    setup_params = function(matrix, params) {
        aes_nms <- setdiff(names(params), "direction")
        l <- lengths(params[aes_nms])
        expected_len <- max(l)
        if (!all(l == 1L | l == expected_len)) {
            cli::cli_abort(c(
                "{.arg {aes_nms}} must have compatible sizes",
                i = "Only values of size one are recycled."
            ))
        }
        c(
            lapply(params[aes_nms], rep_len, length.out = expected_len),
            params["direction"]
        )
    },
    draw_slice = function(self, layer_matrix, aesthetics,
                          band_col, band_fill,
                          band_alpha, band_linewidth,
                          band_linetype, lineend, linejoin,
                          direction = "row") {
        force(band_col)
        force(band_fill)
        force(band_alpha)
        force(band_linewidth)
        force(band_linetype)
        force(direction)
        function(i, x, y, w, h, fill) {
            data <- switch(direction,
                row = cbind(h, y),
                column = cbind(w, x)
            )
            data <- unique(data)
            y <- data[, 2L, drop = TRUE]
            idx <- (order(y) %% length(band_fill)) + 1L
            colour <- band_col[idx]
            fill <- band_fill[idx]
            alpha <- band_alpha[idx]
            linewidth <- band_linewidth[idx]
            linetype <- band_linetype[idx]
            flip_grid(grid::grid.rect,
                which = switch(direction,
                    row = "column",
                    column = "row"
                ),
                y = y,
                height = data[, 1L, drop = TRUE],
                gp = gpar(
                    col = colour,
                    fill = alpha(fill, alpha),
                    lwd = linewidth * .pt,
                    lty = linetype,
                    linejoin = linejoin,
                    lineend = lineend
                ),
                name = "eheat_band"
            )
        }
    }
)
