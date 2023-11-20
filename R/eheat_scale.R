#' Wrap ggplot2 scale object into ComplexHeatmap
#' @param scale A [scale][ggplot2::discrete_scale] object.
#' @param matrix Matrix used for this aesthetic mapping
#' @export
eheat_map <- function(scale = NULL, aesthetic = NULL, matrix = NULL) {
    matrix <- allow_lambda(matrix)
    if (is.null(scale) && is.null(aesthetic)) {
        cli::cli_abort("One of {.arg scale} or {.arg aesthetic} must be provided")
    } else if (!is.null(scale)) { # ignore aesthetic argument
        assert_s3_class(scale, "Scale")
        out <- ggplot2::ggproto("eheatMap", scale, matrix = matrix)
    } else {
        aesthetic <- standardise_aes_names(aesthetic)
        if (!any(aesthetic == ggplot2:::.all_aesthetics)) {
            cli::cli_abort("Invalid {aesthetic} aesthetic")
        }
        out <- ggplot2::ggproto("eheatMap", scale,
            matrix = matrix, aesthetics = aesthetic
        )
    }
    out$aesthetic <- function(self, layer_matrix) {
        name <- self$aesthetics[1L] # nolint
        if (is.null(self$matrix)) {
            matrix <- layer_matrix
        } else {
            if (is.function(self$matrix)) {
                matrix <- self$matrix(layer_matrix)
                if (!is.matrix(matrix)) {
                    cli::cli_abort("{.fn matrix} of {name} aesthetic must return a {.cls matrix}")
                }
            } else {
                matrix <- self$matrix
            }
            # check heat_matrix and layer_matrix are compatible
            if ((!is.null(dim(matrix)) &&
                !all(dim(matrix) == dim(layer_matrix))) ||
                (is.null(dim(matrix)) && is.atomic(matrix) &&
                    length(matrix) != length(layer_matrix))) {
                msg <- sprintf(
                    "(%s) aesthetic matrix",
                    style_fn(snake_class(self))
                )
                msg <- paste(msg,
                    "is not compatible with heatmap matrix",
                    sep = " "
                )
                cli::cli_abort(msg)
            }
        }
        matrix
    }
    out
}

is_eheat_map <- function(x) {
    inherits(x, "eheatMap")
}

is_scale <- function(x) {
    inherits(x, "Scale")
}

# Scales object encapsulates multiple scales.
# All input and output done with data.frames to facilitate
# multiple input and output variables
eheat_map_list <- function() {
    ggplot2::ggproto(NULL, eheatMapList)
}

eheatMapList <- ggplot2::ggproto("eheatMapList", NULL,
    elements = NULL,
    aesthetic_data = function(self, layer_matrix) {
        # Evaluate aesthetics
        data <- lapply(self$elements, function(eheat_map) {
            c(eheat_map$aesthetic(layer_matrix))
        })
        data <- data_frame0(!!!data)
        names(data) <- vapply(
            self$elements,
            function(x) x$aesthetics[1L],
            character(1L)
        )
        data
    },
    find_aes = function(self, aesthetic) {
        vapply(self$elements, function(x) any(aesthetic %in% x$aesthetics), logical(1))
    },
    has_aes = function(self, aesthetic) {
        any(self$find_aes(aesthetic))
    },
    add = function(self, element) {
        if (is.null(element)) {
            return()
        }
        if (!is_eheat_map(element) && is_scale(element)) {
            element <- eheat_map(scale = element)
        }
        prev_aes <- self$find_aes(element$aesthetics)
        if (any(prev_aes)) {
            # Get only the first aesthetic name in the returned vector -- it can
            # sometimes be c("x", "xmin", "xmax", ....)
            mapping_nm <- self$elements[prev_aes][[1]]$aesthetics[1]
            cli::cli_inform(c(
                "Mapping for {.field {mapping_nm}} is already present.",
                "Adding another scale for {.field {mapping_nm}}, which will replace the existing scale."
            ))
        }

        # Remove old scale for this aesthetic (if it exists)
        self$elements <- c(self$elements[!prev_aes], list(element))
    },
    n = function(self) {
        length(self$elements)
    },
    get_scales = function(self, aesthetic) {
        self$elements[vapply(self$elements, is_scale, logical(1))]
    },
    input_mapping = function(self) {
        unlist(lapply(self$elements, `[[`, "aesthetics"))
    },
    input_scales = function(self) {
        unlist(lapply(self$get_scales(), `[[`, "aesthetics"))
    },
    clone_scales = function(self) {
        ggplot2::ggproto(NULL, self, elements = self$get_scales())
    },
    get_mapping = function(self, output) {
        mapping <- self$elements[self$find_aes(output)]
        if (length(mapping) == 0) {
            return()
        }
        mapping[[1]]
    },
    # `aesthetics` is a list of aesthetic-variable mappings. The name of each
    # item is the aesthetic, and the value of each item is the variable in data.
    add_defaults = function(self, env) {
        new_aesthetics <- setdiff(self$input_mapping(), self$input_scales())
        # No new aesthetics, so no new scales to add
        if (is.null(new_aesthetics)) {
            return()
        }

        for (aes in new_aesthetics) {
            self$add(find_scale(aes, self$get_mapping(aes)$matrix, env))
        }
    },

    # used for only scales mapping ----------------------------
    train_df = function(self, df, drop = FALSE) {
        if (empty(df) || length(self$elements) == 0) {
            return()
        }
        lapply(self$elements, function(scale) scale$train_df(df = df))
    },
    map_df = function(self, df) {
        if (empty(df) || length(self$elements) == 0) {
            return(df)
        }

        mapped <- unlist(lapply(
            self$elements,
            function(scale) scale$map_df(df = df)
        ), recursive = FALSE)

        data_frame0(!!!mapped, df[setdiff(names(df), names(mapped))])
    },
    transform_df = function(self, df) {
        if (empty(df)) {
            return(df)
        }

        # If the scale contains to trans or trans is identity, there is no need
        # to transform anything
        idx_skip <- vapply(self$elements, function(x) {
            has_default_transform(x) &&
                (is.null(x$trans) || identical(x$trans$transform, identity))
        }, logical(1L))
        scales <- self$elements[!idx_skip]

        if (length(scales) == 0) {
            return(df)
        }

        transformed <- unlist(lapply(
            scales,
            function(scale) scale$transform_df(df = df)
        ), recursive = FALSE)

        data_frame0(!!!transformed, df[setdiff(names(df), names(transformed))])
    },
    backtransform_df = function(self, df) {
        # NOTE: no need to check empty(df) because it should be already checked
        # before this method is called.

        # If the scale contains to trans or trans is identity, there is no need
        # to transform anything
        idx_skip <- vapply(self$elements, function(x) {
            has_default_transform(x) &&
                (is.null(x$trans) || identical(x$trans$transform, identity))
        }, logical(1))
        scales <- self$elements[!idx_skip]

        if (length(scales) == 0) {
            return(df)
        }

        backtransformed <- unlist(lapply(
            scales,
            function(scale) {
                aesthetics <- intersect(scale$aesthetics, names(df))
                if (length(aesthetics) == 0) {
                    return()
                }
                lapply(df[aesthetics], scale$trans$inverse)
            }
        ), recursive = FALSE)

        data_frame0(
            !!!backtransformed,
            df[setdiff(names(df), names(backtransformed))]
        )
    },

    # Add missing but required scales
    # `aesthetics` is a character vector of aesthetics. Typically c("x", "y")
    add_missing = function(self, aesthetics, env) {
        aesthetics <- setdiff(aesthetics, self$input())

        for (aes in aesthetics) {
            scale_name <- paste("scale", aes, "continuous", sep = "_")
            self$add(find_global(scale_name, env, mode = "function")())
        }
    }
)

find_scale <- function(aes, x, env = parent.frame()) {
    if (is.null(x) || (rlang::is_atomic(x) && all(is.infinite(x)))) {
        return(NULL)
    }
    type <- ggplot2::scale_type(x)
    candidates <- paste("scale", aes, type, sep = "_")
    for (scale in candidates) {
        scale_f <- find_global(scale, env, mode = "function")
        if (!is.null(scale_f)) {
            return(scale_f())
        }
    }
    return(NULL)
}

find_global <- function(name, env, mode = "any") {
    if (exists(name, envir = env, mode = mode)) {
        return(get(name, envir = env, mode = mode))
    }
    nsenv <- asNamespace("ggplot2")
    if (exists(name, envir = nsenv, mode = mode)) {
        return(get(name, envir = nsenv, mode = mode))
    }
    NULL
}

empty <- function(x) {
    is.null(x) || nrow(x) == 0 || ncol(x) == 0
}

data_frame0 <- function(...) {
    tibble::tibble(..., .name_repair = "minimal")
}

has_default_transform <- function(scale) {
    transform_method <- environment(scale$transform)$f
    identical(default_transform, transform_method) ||
        identical(identity, transform_method)
}

default_transform <- function(self, x) {
    self$trans$transform(x)
}
