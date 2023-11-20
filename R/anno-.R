new_anno <- function(
    scale = NULL, # for y-axis
    matrix = NULL, name = NULL, anno_class = eheatAnno) {
    assert_s3_class(scale, "Scale", null_ok = TRUE)
    matrix <- allow_lambda(matrix)
    ggplot2::ggproto("eheatAnnoInstance", anno_class,
        name = name %||% rlang::as_name(rlang::caller_call()[[1L]]),
        matrix = matrix
    )
}

eheatAnno <- ggplot2::ggproto(
    "eheatAnno", NULL,
    name = NULL, matrix = NULL, scale = NULL,
    # Initialise anno matrix
    anno_matrix = function(self, which, heat_matrix) {
        if (is.null(self$matrix) && is.null(heat_matrix)) {
            cli::cli_abort("{.arg matrix} must be provided")
        } else if (is.null(self$matrix)) {
            matrix <- heat_matrix
        } else {
            if (is.function(self$matrix)) {
                matrix <- self$matrix(heat_matrix)
            } else {
                matrix <- self$matrix
            }
            matrix <- build_matrix(matrix)
            if (!is.null(heat_matrix)) {
                # check heat_matrix and anno_matrix are compatible
                bad_matrix <- switch(which,
                    row = nrow(matrix) == nrow(heat_matrix),
                    column = nrow(matrix) == ncol(heat_matrix)
                )
                if (bad_matrix) {
                    msg <- sprintf(
                        "(%s) annotation matrix",
                        style_fn(self$name)
                    )
                    msg <- paste(msg, "is not compatible with heatmap matrix",
                        sep = " "
                    )
                    cli::cli_abort(msg)
                }
            }
        }
        matrix
    },
    get_limits = function(self, matrix) {
        if (is.null(self$scale)) {
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
    },
    draw_anno = function(self, draw_fn, ...,
                         subset_rule = NULL, subsettable = NULL,
                         width = NULL, height = NULL, show_name = TRUE,
                         which = NULL, matrix = NULL, heat_matrix = NULL) {
        name <- self$name
        if (ht_opt$verbose) {
            cli::cli_inform("construct AnnotationFunction with {.fn {name}}")
        }
        anno <- methods::new("AnnotationFunction")
        which <- cheat_which(which)
        anno@which <- which
        matrix <- self$anno_matrix(which, heat_matrix)
        anno@fun_name <- name
        anno_size <- anno_width_and_height(which, width, height, unit(1, "cm"))
        anno@width <- anno_size$width
        anno@height <- anno_size$height
        anno@show_name <- show_name
        n <- nrow(matrix)
        anno@n <- n
        yscale <- self$get_limits(matrix)
        anno@data_scale <- yscale
        draw_fn <- allow_lambda(draw_fn)
        dots <- rlang::list2(...)
        if (length(dots) != sum(nzchar(names(dots)))) {
            cli::cli_abort("All members in {.arg ...} must be named.")
        }
        anno@fun <- function(index, k, n) {
            vp <- flip_viewport(which,
                xscale = c(0.5, n + 0.5),
                yscale = yscale
            )
            matrix <- matrix[index, , drop = FALSE]
            rlang::inject(draw_fn(matrix, !!!dots, which = which, vp = vp))
        }
        anno@var_env <- new.env(parent = environment())
        anno@subsettable <- subsettable %||% TRUE
        var_import <- list(
            matrix = matrix, dots = dots,
            which = which, yscale = yscale
        )
        if (isTRUE(anno@subsettable)) {
            internal_subset <- list(
                matrix = function(x, i) x[i, , drop = FALSE]
            )
            if (length(dots) && is.null(subset_rule)) {
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
        anno@subset_rule <- subset_rule %||% list()
        anno
    }
)

anno_width_and_height <- function(which, width = NULL, height = NULL, default = unit(10, "mm")) {
    # height must be absolute
    params <- list(width = width, height = height)
    if (which == "row") {
        params <- flip_gp(params)
        arg <- "width" # nolint
    } else {
        arg <- "height"
    }
    if (is.null(params$height)) {
        params$height <- default
    } else if (!ComplexHeatmap::is_abs_unit(params$height)) {
        cli::cli_abort(
            "{.arg {arg}} of the annotation can only be an absolute unit."
        )
    }
    if (is.null(params$width)) {
        params$width <- unit(1L, "npc")
    }
    if (which == "row") flip_gp(params) else params
}

is_discrete <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
}
