#' Subsettable AnnotationFunction
#'
#' @inheritParams ggheat
#' @param draw_fn A function which defines how to draw the annotation. The
#' function should accept at least `matrix`, `index`, `k`, `n` arguments.
#' `...` is also passed on to this function.
#' @param ... Additional arguments passed on to `draw_fn`.
#' @inheritParams new_anno
#' @inheritParams ComplexHeatmap::AnnotationFunction
#' @param subset_rule A list of function to subset variables in `...`.
#' @details
#' `new_anno_subset` is similar with
#' [AnnotationFunction][ComplexHeatmap::AnnotationFunction], but
#' `new_anno_subset` won't change the function environment of `draw_fn`. So it's
#' safe to use `new_anno_subset` in pacakge development, particularly when
#' dealing with internal functions in the package namespace that are likely to
#' exist. You must always ensure arguments passed on to `...` have subset rules
#' since the internal will set `@@subsettable` to `TRUE`.
#' @examples
#' anno <- new_anno_subset(
#'     rnorm(10L),
#'     draw_fn = function(matrix, index, k, n) {
#'         n <- length(index)
#'         pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(0, 10)))
#'         grid.rect()
#'         grid.points(1:n,
#'             matrix[index, , drop = FALSE],
#'             default.units = "native"
#'         )
#'         if (k == 1) grid.yaxis()
#'         popViewport()
#'     },
#'     height = unit(2, "cm")
#' )
#' draw(anno)
#' @seealso
#' - [new_anno]
#' - [AnnotationFunction][ComplexHeatmap::AnnotationFunction]
#' @inherit ComplexHeatmap::AnnotationFunction return
#' @export
new_anno_subset <- function(
    matrix, draw_fn, ..., ylim = NULL, subset_rule = NULL,
    width = NULL, height = NULL, show_name = TRUE,
    which = NULL, name = NULL) {
    matrix <- build_matrix(matrix)
    draw_fn <- allow_lambda(draw_fn)
    if (...length() != sum(nzchar(...names()))) {
        cli::cli_abort("All elements in {.arg ...} must be named.")
    }
    name <- name %||% "anno_subset"
    dots <- rlang::list2(...)
    # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/AnnotationFunction-class.R#L270
    internal_subset <- list(matrix = function(x, i) x[i, , drop = FALSE])
    if (...length() && is.null(subset_rule)) {
        subset_rule <- lapply(dots, function(var) {
            if (is.matrix(var)) {
                function(x, i) x[i, , drop = FALSE]
            } else if (inherits(var, "gpar")) {
                subset_gp
            } else if (is.vector(var)) {
                if (length(var) > 1) function(x, i) x[i]
            }
        })
    }
    if (length(subset_rule)) {
        rules <- subset_rule
        rules_nms <- names(rules)
        subset_rule <- list(dots = function(x, i) {
            imap(x, function(element, nm) {
                if (any(nm == rules_nms)) {
                    rule <- rules[[nm]]
                    if (is.null(rule) || isFALSE(rule)) {
                        # Don't do subset
                        element
                    } else {
                        # subset element
                        rule(element, i)
                    }
                } else {
                    element
                }
            })
        })
    }
    subset_rule <- c(internal_subset, subset_rule)
    which <- cheat_which(which)
    anno <- new_anno(
        n = nrow(matrix),
        draw_fn = function(index, k, n) {
            rlang::inject(draw_fn(
                matrix = matrix, !!!dots,
                index = index, k = k, n = n
            ))
        },
        ylim = ylim,
        which = which, width = width, height = height,
        show_name = show_name, name = name
    )
    anno@subsettable <- TRUE
    anno@subset_rule <- subset_rule
    # we change `var_env` into the environment of `draw_fn`
    anno@var_env <- environment(anno@fun)
    # Only save necessary variables for usage of `draw_fn`
    on.exit(rm(
        list = setdiff(
            ls(envir = anno@var_env, all.names = TRUE),
            c("draw_fn", "matrix", "dots")
        ),
        envir = anno@var_env, inherits = FALSE
    ))
    anno
}

anno_check_matrix <- function(matrix, which, heat_matrix, name) {
    if (is.null(matrix) && is.null(heat_matrix)) {
        cli::cli_abort("{.arg matrix} must be provided")
    } else if (is.null(matrix)) {
        matrix <- heat_matrix
    } else {
        if (is.function(matrix)) {
            matrix <- matrix(heat_matrix)
        }
        matrix <- build_matrix(matrix)
        if (!is.null(heat_matrix)) {
            # check heat_matrix and anno_matrix are compatible
            bad_matrix <- switch(which,
                row = nrow(matrix) == nrow(heat_matrix),
                column = nrow(matrix) == ncol(heat_matrix)
            )
            if (bad_matrix) {
                msg <- sprintf("(%s) annotation matrix", style_fn(name))
                msg <- paste(msg, "is not compatible with heatmap matrix",
                    sep = " "
                )
                cli::cli_abort(msg)
            }
        }
    }
    matrix
}

scale_get_limits <- function(matrix, scale = NULL) {
    if (is.null(scale)) {
        if (is_discrete(matrix)) {
            scale <- ggplot2::scale_y_discrete()
        } else {
            scale <- ggplot2::scale_y_continuous()
        }
    }
    new_scale <- scale$clone()
    new_scale$reset()
    new_scale$train(matrix)
    new_scale$get_limits()
}
