new_layer <- function(geom, ..., matrix = NULL, check.aes = TRUE, check.param = TRUE, name = NULL, layer_class = eheatLayer) {
    call_env <- rlang::caller_env()
    dots <- rlang::list2(...)
    scale_idx <- vapply(dots, is_eheat_scale, logical(1L))
    scales <- dots[scale_idx]
    if (length(scales) != sum(nzchar(names(scales)))) {
        cli::cli_abort("All {.cls eheatScale} objects must be named",
            call = call_env
        )
    }
    params <- dots[!scale_idx]
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

    extra_aes <- setdiff(names(scales), geom$aesthetics_nms())
    if (check.aes && length(extra_aes) > 0) {
        cli::cli_warn("Ignoring unknown aesthetics: {.field {extra_aes}}",
            call = call_env
        )
    }
    if (!is.null(matrix)) matrix <- build_matrix(matrix)
    ggplot2::ggproto("eheatLayerInstance", layer_class,
        name = name, matrix = matrix,
        scales = scales,
        geom = geom, geom_params = geom_params,
        aes_params = aes_params
    )
}

is_eheat_layer <- function(x) {
    inherits(x, "eheatLayer")
}

eheatLayer <- ggplot2::ggproto(
    "eheatLayer", NULL,
    name = NULL, matrix = NULL,
    scales = NULL, # for heatmap, the geom internally can have their own scales
    geom = NULL, geom_params = NULL, aes_params = NULL,
    # Combine `aesthetics` with defaults and set aesthetics from parameters
    # params directly set aesthetic in values
    setup_aesthetics = function(self, heat_matrix) {
        check_required_aesthetics(
            self$geom$required_aes,
            c(names(self$scales), names(self$aes_params)),
            self$name
        )
        layer_matrix <- self$matrix %||% heat_matrix
        aesthetics <- self$map_aesthetics(layer_matrix)
        right_length <- length(layer_matrix)
        right_dim <- dim(layer_matrix)
        # Override mappings with params
        params <- self$aes_params
        if (length(params)) {
            check_aesthetics(params, right_length, right_dim, "layer")
            for (aes_nm in names(params)) {
                aesthetics[[aes_nm]] <- rep_len(params[[aes_nm]], right_length)
            }
        }
        # Fill in missing aesthetics with their defaults
        default_aes <- self$geom$default_aes
        missing_aes_nms <- setdiff(names(default_aes), names(aesthetics))

        # Needed for geoms with defaults set to NULL (e.g. GeomSf)
        missing_aes <- compact(default_aes[missing_aes_nms])
        if (length(missing_aes)) {
            check_aesthetics(missing_aes, right_length, right_dim, "layer")
            missing_aesthetics <- lapply(missing_aes, function(missing) {
                rep_len(missing, right_length)
            })
            aesthetics <- c(aesthetics, missing_aesthetics)
        }
        aesthetics
    },
    # `id` used for message
    draw_layer = function(self, heat_matrix, aesthetics) {
        layer_matrix <- self$matrix %||% heat_matrix
        geom_params <- self$geom_params
        # Trim off extra parameters
        geom_params <- geom_params[
            intersect(names(geom_params), self$geom$parameters())
        ]
        # firstly, do some preparation for params
        geom_params <- self$geom$setup_params(layer_matrix, geom_params)
        # then, do some preparation for matrix
        layer_matrix <- self$geom$setup_matrix(layer_matrix, geom_params)
        rlang::inject(self$geom$draw_slice(
            layer_matrix, aesthetics, !!!geom_params
        ))
    },
    map_aesthetics = function(self, layer_matrix) {
        if (length(self$scales)) {
            imap(self$scales, function(scale, id) {
                scale$map_aesthetic(layer_matrix, id)
            })
        } else {
            list()
        }
    },
    draw_guides = NULL
)

check_aesthetics <- function(x, length, dim, name) {
    good <- vapply(x, function(mat) {
        cur_len <- length(mat)
        cur_dim <- dim(mat)
        if (is.null(cur_dim)) {
            cur_len == 1L || cur_len == length
        } else {
            cur_len == length && all(dim(mat) == dim)
        }
    }, logical(1L))
    if (all(good)) {
        return()
    }
    cli::cli_abort(c(
        "Aesthetics must be compatible with {name} matrix ({paste0(dim, collapse = \" x \")})",
        "x" = "Fix the following aesthetics: {.col {names(which(!good))}}"
    ))
}
