eheatGeom <- ggplot2::ggproto("eheatGeom",
    required_aes = character(),
    non_missing_aes = character(),
    optional_aes = character(),
    default_aes = list(),
    draw_key = NULL,
    setup_matrix = function(matrix, params) matrix,
    setup_params = function(matrix, params) params,

    # Combine `aesthetics` with defaults and set aesthetics from parameters
    # params directly set aesthetic in values
    setup_aesthetics = function(self, layer_matrix, aesthetics, params) {
        right_length <- length(layer_matrix)
        right_dim <- dim(layer_matrix)
        # Override mappings with params
        if (length(params)) {
            check_aesthetics(params, right_length, right_dim, "layer")
            for (aes_nm in names(params)) {
                aes_param <- rep_len(params[[aes_nm]], right_length)
                dim(aes_param) <- right_dim
                aesthetics[[aes_nm]] <- aes_param
            }
        }
        # Fill in missing aesthetics with their defaults
        default_aes <- self$default_aes
        missing_aes_nms <- setdiff(names(default_aes), names(aesthetics))

        # Needed for geoms with defaults set to NULL (e.g. GeomSf)
        missing_aes <- compact(default_aes[missing_aes_nms])
        if (length(missing_aes)) {
            check_aesthetics(missing_aes, right_length, right_dim, "layer")
            missing_aesthetics <- lapply(missing_aes, function(missing) {
                out <- rep_len(missing, right_length)
                dim(out) <- right_dim
                out
            })
            aesthetics <- c(aesthetics, missing_aesthetics)
        }
        aesthetics
    },
    draw_geom = function(self, layer_matrix, aesthetics, geom_params, aes_params) {
        # Trim off extra parameters
        geom_params <- geom_params[
            intersect(names(geom_params), self$parameters())
        ]
        # firstly, do some preparation for params
        geom_params <- self$setup_params(layer_matrix, geom_params)
        # then, do some preparation for matrix
        layer_matrix <- self$setup_matrix(layer_matrix, geom_params)
        # Prepare all aesthetic matrix into a list
        aesthetics <- self$setup_aesthetics(
            layer_matrix, aesthetics, aes_params
        )
        rlang::inject(self$draw_slice(layer_matrix, aesthetics, !!!geom_params))
    },
    draw_slice = function(self, layer_matrix, aesthetics, ...) {
        cell_fn <- self$draw_cell(aesthetics, ...)
        function(j, i, x, y, width, height, fill) {
            .mapply(cell_fn, list(j, i, x, y, width, height, fill), NULL)
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
    # draw_cell(). However, some additional parameters may be needed
    # for setup_data() or handle_na(). These can not be imputed automatically,
    # so the slightly hacky "extra_params" field is used instead. By
    # default it contains `na.rm`
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

check_aesthetics <- function(x, length, dim, name) {
    ns <- lengths(x)
    good <- ns == 1L |
        (ns == length & all(vapply(x, function(aes) {
            cur_dim <- dim(aes)
            !is.null(cur_dim) && all(cur_dim == dim)
        }, logical(1L))))


    if (all(good)) {
        return()
    }

    cli::cli_abort(c(
        "Aesthetics must be either length 1 or the same dimention as the {name} matrix ({paste0(dim, collapse = \" x \")})",
        "x" = "Fix the following aesthetics: {.col {names(which(!good))}}"
    ))
}
