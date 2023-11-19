eheatGeom <- ggplot2::ggproto("eheatGeom",
    required_aes = character(),
    non_missing_aes = character(),
    optional_aes = character(),
    default_aes = list(),
    draw_key = NULL,
    setup_params = function(matrix, params) params,
    setup_data = function(matrix, params) matrix,
    use_defaults = function(self, data, params = list()) {
        # Override mappings with params
        aes_params <- intersect(self$aesthetics_nms(), names(params))
        if (length(aes_params)) {
            check_aesthetics(params[aes_params], nrow(data))
            data[aes_params] <- params[aes_params]
        }

        # Fill in missing aesthetics with their defaults
        default_aes <- self$default_aes
        missing_aes <- setdiff(names(default_aes), names(data))

        # Needed for geoms with defaults set to NULL (e.g. GeomSf)
        missing_aes <- compact(default_aes[missing_aes])

        if (length(missing_aes)) {
            check_aesthetics(missing_aes, nrow(data))
            data[names(missing_aes)] <- missing_aes
        }
        data
    },
    handle_na = function(self, data, params) {
        ggplot2::remove_missing(
            data, params$na.rm,
            c(self$required_aes, self$non_missing_aes),
            snake_class(self)
        )
    },
    draw_layer = function(self, data, group, params) {
        # Trim off extra parameters
        params <- params[intersect(names(params), self$parameters())]
        data_list <- split(data, ~.slice_group)
        lapply(data_list, function(data) {
            rlang::inject(self$draw_slice(data, group, !!!params))
        })
    },
    draw_slice = function(self, data, group) {
        cli::cli_abort("{.fn {snake_class(self)}}, has not implemented a {.fn draw_slice} method")
    },
    aesthetics_nms = function(self) {
        if (is.null(self$required_aes)) {
            required_aes <- NULL
        } else {
            required_aes <- unlist(strsplit(self$required_aes, "|", fixed = TRUE))
        }
        c(union(required_aes, names(self$default_aes)), self$optional_aes)
    },
    # Most parameters for the geom are taken automatically from draw_slice() or
    # draw_cell(). However, some additional parameters may be needed for
    # setup_data(). These can not be imputed automatically, so the slightly
    # hacky "extra_params" field is used instead.
    extra_params = "na.rm",
    parameters = function(self, extra = FALSE) {
        # Look first in draw_panel. If it contains ... then look in draw groups
        args <- names(ggproto_formals(self$draw_slice))

        # Remove arguments of defaults
        args <- setdiff(args, names(ggproto_formals(eheatGeom$draw_slice)))

        if (extra) {
            args <- union(args, self$extra_params)
        }
        args
    },
)
