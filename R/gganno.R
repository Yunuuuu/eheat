gganno <- function(
    matrix, ggfn, ..., which = NULL,
    width = NULL, height = NULL, debug = FALSE) {
    matrix <- build_matrix(matrix)
    row_nms <- rownames(matrix)
    data <- tibble::as_tibble(matrix, .name_repair = "unique") # nolint
    ggfn <- allow_lambda(ggfn)
    debug <- allow_lambda(debug)
    env <- new.env() # nolint
    dots <- rlang::list2(...)
    draw_fn <- function(index, k, n) {
        if (k == 1L) {
            # only prepare ggplot data in the first run and run everytime when
            # draw function execution
            # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/HeatmapList-draw_component.R
            # trace back into `draw_heatmap_list()`
            order_list <- cheat_get_order_list("ht_main")
            order_list <- switch(which,
                row = order_list$row_order_list,
                column = order_list$column_order_list
            )
            if (length(order_list) > 1L) {
                env$with_slice <- TRUE
            } else {
                env$with_slice <- FALSE
            }
            x <- unlist(order_list,
                recursive = FALSE, use.names = FALSE
            )
            update_x <- data_frame0(
                .slice = rep(
                    seq_along(order_list),
                    times = lengths(order_list)
                ),
                x = unlist(lapply(order_list, seq_along),
                    recursive = FALSE, use.names = FALSE
                )
            )
            data$index <- seq_len(nrow(data))
            data <- dplyr::bind_cols(
                update_x, data[match(x, data$index), ],
                .name_repair = "minimal"
            )
            p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x))
            p <- rlang::inject(ggfn, !!!dots)
            if (!ggplot2::is.ggplot(p)) {
                cli::cli_abort(
                    "{.arg ggfn} must return a {.cls ggplot2} object."
                )
            }
            p <- p + ggplot2::scale_x_continuous(
                name = NULL,
                limits = c(0.5, nrow(data) + 0.5),
                breaks = seq_len(nrow(data)),
                labels = row_nms,
                expand = ggplot2::expansion()
            )
            if (which == "row") {
                p <- p + ggplot2::coord_flip()
                facet_params <- list(
                    rows = ggplot2::vars(.data$.slice),
                    scales = "free_y", space = "free_y"
                )
            } else {
                facet_params <- list(
                    cols = ggplot2::vars(.data$.slice),
                    scales = "free_x", space = "free_x"
                )
            }
            if (env$with_slice) {
                p <- p + do.call(ggplot2::facet_grid, facet_params)
            }
            if (isTRUE(debug)) {
                rlang::return_from(sys.frame(which = 1L), value = p)
            } else if (is.function(debug)) {
                debug(p)
            }
            env$p <- p
            env$gt <- ggplot2::ggplotGrob(p)
        }
        vp <- flip_viewport(which, xscale = c(0.5, n + 0.5), yscale = c(0, 1))
        if (env$with_slice) {
            if (which == "row") {
                pattern <- c("panel-%d-1")
                if (k == 1L) {
                    pattern <- c(pattern, "axis-t", "lab-t")
                } else if (k == n) {
                    pattern <- c(pattern, "axis-b", "lab-b")
                }
            } else {
                pattern <- "panel-1-%d"
                if (k == 1L) {
                    pattern <- c(pattern, "axis-l", "lab-l")
                } else if (k == n) {
                    pattern <- c(pattern, "axis-r", "lab-r")
                }
            }
            pattern <- paste0(sprintf(pattern, k), collapse = "|")
            fit_panel(
                trim_zero_grob(gtable::gtable_filter(env$gt, pattern)),
                vp = vp
            )
        } else {
            fit_panel(trim_zero_grob(env$gt), vp = vp)
        }
    }
    new_anno(
        n = nrow(data), draw_fn = draw_fn, ylim = NULL,
        subset_rule = list(), subsettable = FALSE,
        which = which, width = width, height = height,
        show_name = FALSE, name = "gganno"
    )
}

# ComplexHeatmap::Legend()
# legend <- function() {
#   object <- new("Legends")
#   object@grob <- legend_body
#   object@type <- "single_legend_no_title"
#   object@n <- 1
#   object@multiple <- 1
#   object@direction <- "vertical"
#   return(object)
#   object <- new("Legends")
#   object@grob <- legend_body
#   object@type <- "single_legend_no_title"
#   object@n <- 1
#   object@multiple <- 1
#   object@direction <- "vertical"
#   return(object)
#   object <- new("Legends")
#   object@grob <- gf
#   object@type <- "single_legend"
#   object@name <- name
#   object@n <- 1
#   object@multiple <- 1
#   object@direction <- "vertical"
# }
