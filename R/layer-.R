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
    # `id` used for message
    draw_layer = function(self, heat_matrix, id = NULL) {
        if (!is.null(self$matrix) &&
            !all(dim(self$matrix) == dim(heat_matrix))) {
            if (!is.null(self$name)) {
                msg <- "Layer matrix"
            } else {
                msg <- sprintf("%s layer matrix", self$name)
            }

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
        check_required_aesthetics(
            self$geom$required_aes,
            c(names(self$scales), names(self$aes_params)),
            self$name
        )
        layer_matrix <- self$matrix %||% heat_matrix
        self$geom$draw_geom(
            layer_matrix, self$scales,
            geom_params = self$geom_params,
            aes_params = self$aes_params
        )
    },
    draw_guides = NULL
)
