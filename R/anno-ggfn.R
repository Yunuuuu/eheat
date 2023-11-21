anno_ggfn <- function(ggfn, ..., debug = FALSE) {
    ggfn <- allow_lambda(ggfn)
    debug <- allow_lambda(debug)
    draw_fn <- function(matrix, ..., which, vp, debug) {
        data <- tibble::as_tibble(build_matrix(matrix), .name_repair = "unique")
        data$x <- seq_len(nrow(data))
        p <- ggfn(data, ...)
        if (isTRUE(debug)) {
            rlang::return_from(sys.frame(which = 1L), value = p)
        } else if (is.function(debug)) {
            debug(p)
        }
        if (!ggplot2::is.ggplot(p)) {
            cli::cli_abort("{.arg ggfn} must return a {.cls ggplot2} object.")
        }
        p <- p + ggplot2::scale_x_discrete(
            name = NULL,
            expand = ggplot2::expansion(add = 0.5)
        )
        if (which == "row") {
            p <- p + ggplot2::coord_flip()
        }
        fit_panel(p, vp = vp)
    }
    anno_fn(
        draw_fn = draw_fn, ..., show_name = FALSE,
        debug = debug, name = "anno_ggfn"
    )
}
