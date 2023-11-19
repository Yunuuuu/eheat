new_layer <- function(geom, ..., group = NULL, fun = mean, fun.args = list(), matrix = NULL, check.aes = TRUE, check.param = TRUE, name = NULL, layer_class = eheatLayer) {
    call_env <- rlang::caller_env()
    dots <- rlang::list2(...)
    map_idx <- vapply(dots, is_eheat_map, logical(1L))
    map_list <- dots[map_idx]

    params <- dots[!map_idx]
    # Split up params between aesthetics and geom
    params <- rename_aes(params)
    params_nms <- names(params)
    geom_params <- params[intersect(params_nms, geom$parameters(TRUE))]
    aes_params <- params[intersect(params_nms, geom$aesthetics_nms())]
    bad_aes <- vapply(
        aes_params,
        function(x) inherits(x, "Scale"), logical(1L)
    )
    if (any(bad_aes)) {
        cli::cli_abort(c(
            "Found {.cls Scale} object directly set in {.field {names(bad_aes[bad_aes])}} aesthetic",
            i = "Did you forget wrap it with {.fn eheat_scale}?"
        ))
    }
    all <- c(geom$parameters(TRUE), geom$aesthetics_nms())

    # Warn about extra params and aesthetics
    extra_param <- setdiff(names(params), all)
    if (check.param && length(extra_param) > 0) {
        cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}",
            call = call_env
        )
    }

    extra_aes <- setdiff(names(map_list), geom$aesthetics_nms())
    if (check.aes && length(extra_aes) > 0) {
        cli::cli_warn("Ignoring unknown aesthetics: {.field {extra_aes}}",
            call = call_env
        )
    }
    matrix <- allow_lambda(matrix)
    # for heatmap, the geom internally can have their own scales
    mapping <- eheat_map_list()
    for (i in seq_along(map_list)) {
        mapping$add(map_list[[i]])
    }
    ggplot2::ggproto("eheatLayerInstance", layer_class,
        name = name, matrix = matrix,
        mapping = mapping, geom = geom, geom_params = geom_params,
        aes_params = aes_params,
        group = group, fun = fun, fun.args = fun.args
    )
}

is_eheat_layer <- function(x) {
    inherits(x, "eheatLayer")
}

eheatLayer <- ggplot2::ggproto(
    "eheatLayer", NULL,
    name = NULL, matrix = NULL,
    group = NULL, fun = mean, fun.args = list(),
    mapping = NULL, geom = NULL, geom_params = NULL, aes_params = NULL,
    # Combine `aesthetics` with defaults and set aesthetics from parameters
    # params directly set aesthetic in values
    # These two fields carry state throughout rendering but will always be
    # calculated before use
    computed_geom_params = NULL,
    computed_stat_params = NULL,

    # Initialise layer matrix
    layer_matrix = function(self, heat_matrix) {
        if (is.null(self$matrix)) {
            matrix <- heat_matrix
        } else {
            if (is.function(self$matrix)) {
                matrix <- self$matrix(heat_matrix)
                if (!is.matrix(matrix)) {
                    cli::cli_abort("{.fn layer_matrix} must return a {.cls matrix}")
                }
            } else {
                matrix <- self$matrix
            }
            matrix <- build_matrix(matrix)
            # check heat_matrix and layer_matrix are compatible
            if (!all(dim(matrix) == dim(heat_matrix))) {
                msg <- sprintf(
                    "(%s) layer matrix",
                    style_fn(snake_class(layer))
                )
                msg <- paste(msg,
                    "is not compatible with heatmap matrix",
                    sep = " "
                )
                cli::cli_abort(msg)
            }
        }
        matrix
    },
    setup_aesthetics = function(self, layer_matrix, envir = parent.frame()) {
        # Evaluate aesthetics
        data <- self$mapping$aesthetic_data(layer_matrix)

        # Drop aesthetics that are set
        data <- data[!names(data) %in% names(self$aes_params)]

        self$mapping$add_defaults(envir)

        data
    },

    # Do summary across row, column or slice
    compute_aesthetics = function(self, aesthetics, slice) {
        out <- imap(slice, function(idx, slice_idx) {
            .raw_idx <- idx[[1L]] * idx[[2L]]
            slice_data <- aesthetics[.raw_idx, , drop = FALSE]
            slice_data$.raw_idx <- .raw_idx
            slice_data$.slice_group <- slice_idx
            slice_data
        })
        do.call("rbind", out)
    },
    compute_geom_1 = function(self, data) {
        if (empty(data)) {
            return(data_frame0())
        }
        check_required_aesthetics(
            self$geom$required_aes,
            c(names(data), names(self$aes_params)),
            snake_class(self$geom)
        )
        self$computed_geom_params <- self$geom$setup_params(
            data, c(self$geom_params, self$aes_params)
        )
        self$geom$setup_data(data, self$computed_geom_params)
    },
    compute_geom_2 = function(self, data) {
        # Combine aesthetics, defaults, & params
        if (empty(data)) {
            return(data)
        }
        self$geom$use_defaults(data, self$computed_geom_params)
    },
    draw_fun = function(self, data, slice) {
        data <- self$geom$handle_na(data, self$computed_geom_params)
        self$geom$draw_layer(data,
            group = self$group, self$computed_geom_params
        )
    },
    draw_guides = NULL
)

check_aesthetics <- function(x, n) {
    ns <- lengths(x)
    good <- ns == 1L | ns == n
    if (all(good)) {
        return()
    }
    cli::cli_abort(c("Aesthetics must be either length 1 or the same as the data ({n})",
        x = "Fix the following mappings: {.col {names(which(!good))}}"
    ))
}
