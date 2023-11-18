eheatGeom <- ggplot2::ggproto("eheatGeom",
    required_aes = character(),
    non_missing_aes = character(),
    optional_aes = character(),
    default_aes = list(),
    draw_key = NULL,
    setup_matrix = function(matrix, params) matrix,
    setup_params = function(matrix, params) params,
    draw_slice = function(self, layer_matrix, aesthetics, ...) {
        cell_fn <- self$draw_cell(layer_matrix, aesthetics, ...)
        function(i, x, y, width, height, fill) {
            .mapply(cell_fn, list(i, x, y, width, height, fill), NULL)
        }
    },
    draw_cell = function(self, layer_matrix, aesthetics) {
        cli::cli_abort("{.fn {class(self)[1L]}}, has not implemented a {.fn draw_cell} method")
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
    # setup_matrix(). These can not be imputed automatically, so the slightly
    # hacky "extra_params" field is used instead.
    extra_params = character(),
    parameters = function(self, extra = FALSE) {
        # Look first in draw_slice. If it contains ... then look in draw groups
        slice_args <- names(ggproto_formals(self$draw_slice))
        cell_args <- names(ggproto_formals(self$draw_cell))
        args <- if ("..." %in% slice_args) cell_args else slice_args

        # Remove arguments of defaults
        args <- setdiff(args, names(ggproto_formals(eheatGeom$draw_cell)))

        if (extra) {
            args <- union(args, self$extra_params)
        }
        args
    },
)
