eheat <- function(matrix, ..., na.rm = FALSE) {
    assert_matrix(matrix)
    dots <- rlang::list2(...)
    layers_idx <- vapply(dots, is_eheat_layer, logical(1L))
    layers <- dots[layers_idx]
    params <- dots[!layers_idx]
    rlang::inject(ComplexHeatmap::Heatmap(
        matrix = matrix,
        rect_gp = gpar(type = "none"),
        !!!params,
        layer_fun = eheat_build(layers, heat_matrix = matrix, na.rm = na.rm),
        show_heatmap_legend = FALSE
    ))
}

eheat_build <- function(layers, heat_matrix, na.rm, finite = FALSE) {
    if (is.null(layers)) {
        return(NULL)
    }
    heat_elements <- .mapply(function(layer, id) {
        id <- if (nzchar(id)) NULL else id
        name <- sprintf("(%s)", style_fn(snake_class(layer)))
        # check heat_matrix and layer_matrix are compatible
        if (!is.null(layer$matrix) &&
            !all(dim(layer$matrix) == dim(heat_matrix))) {
            msg <- sprintf("%s layer matrix", name)
            if (!is.null(id)) {
                msg <- paste(msg, "from", id, sep = " ")
            } else {
                msg <- paste(msg, "provided", sep = " ")
            }
            msg <- paste(msg,
                "is not compatible with heatmap matrix",
                sep = " "
            )
            cli::cli_abort(msg)
        }
        aesthetics <- layer$setup_aesthetics(heat_matrix)
        draw_fn <- layer$draw_layer(heat_matrix, aesthetics)
        # handle NA values
        non_missing_aes <- intersect(
            layer$geom$non_missing_aes,
            names(aesthetics)
        )
        if (length(non_missing_aes)) {
            missing <- !Reduce(`&`, lapply(non_missing_aes, function(var) {
                if (finite) {
                    is_finite(aesthetics[[var]])
                } else {
                    is_complete(aesthetics[[var]])
                }
            }))
        } else {
            missing <- rep_len(FALSE, length(heat_matrix))
        }
        if (any(missing)) {
            if (!na.rm) {
                msg <- paste0(
                    "Will removing {sum(missing)} squares containing ",
                    if (finite) {
                        "non-finite"
                    } else {
                        "missing"
                    }, " values ", name, "."
                )
                cli::cli_warn(msg)
            }
        }
        list(draw_fn = draw_fn, missing = which(missing))
    }, list(layers, rlang::names2(layers)), NULL)
    function(j, i, x, y, w, h, fill) {
        ii <- j * i
        lapply(heat_elements, function(element) {
            element$draw_fn(setdiff(ii, element$missing), x, y, w, h, fill)
        })
    }
}

is_finite <- function(x) {
    if (typeof(x) == "list") {
        !vapply(x, is.null, logical(1))
    } else if (typeof(x) == "character") {
        !is.na(x)
    } else {
        is.finite(x)
    }
}

is_complete <- function(x) {
    if (typeof(x) == "list") {
        !vapply(x, is.null, logical(1))
    } else {
        !is.na(x)
    }
}
