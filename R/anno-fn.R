anno_fn <- function(
    matrix, draw_fn, ..., yscale = NULL,
    subset_rule = NULL, subsettable = NULL,
    width = NULL, height = NULL, show_name = TRUE,
    which = NULL, name = NULL, heat_matrix = NULL) {
    assert_s3_class(yscale, "Scale", null_ok = TRUE)
    matrix <- anno_check_matrix(allow_lambda(matrix), which, heat_matrix)
    ylim <- scale_get_limits(matrix, yscale)
    draw_fn <- allow_lambda(draw_fn)
    if (...length() != sum(nzchar(...names()))) {
        cli::cli_abort("All elements in {.arg ...} must be named.")
    }
    name <- name %||% "anno_fn"
    subsettable <- subsettable %||% TRUE
    dots <- rlang::list2(...)
    if (isTRUE(subsettable)) {
        internal_subset <- list(
            matrix = function(x, i) x[i, , drop = FALSE]
        )
        if (...length() && is.null(subset_rule)) {
            subset_rule <- lapply(dots, function(var) {
                if (is.matrix(var)) {
                    function(x, i) {
                        x[i, , drop = FALSE]
                    }
                } else if (inherits(var, "gpar")) {
                    subset_gp
                } else if (is.vector(var)) {
                    if (length(var) > 1) {
                        function(x, i) {
                            x[i]
                        }
                    }
                }
            })
        }
        if (!is.null(subset_rule)) {
            rules <- subset_rule
            rules_nms <- names(rules)
            subset_rule <- list(dots = function(x, i) {
                imap(x, function(element, nm) {
                    if (any(nm == rules_nms)) {
                        rule <- rules[[nm]]
                        if (is.null(rule) || isFALSE(rule)) {
                            element
                        } else {
                            rule(element, i)
                        }
                    } else {
                        element
                    }
                })
            })
            subset_rule <- c(internal_subset, subset_rule)
        }
    }
    # var_import <- list(
    #     matrix = matrix, dots = dots,
    #     which = which, ylim = ylim
    # )
    new_anno(
        n = nrow(matrix),
        draw_fn = function(index, k, n) {
            vp <- flip_viewport(which,
                xscale = c(0.5, n + 0.5),
                yscale = ylim
            )
            matrix <- matrix[index, , drop = FALSE]
            rlang::inject(draw_fn(matrix, !!!dots, which = which, vp = vp))
        },
        ylim = ylim,
        subset_rule = subset_rule, subsettable = subsettable,
        which = which, width = width, height = height,
        show_name = show_name, name = name
    )
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
