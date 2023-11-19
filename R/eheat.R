eheat <- function(matrix, ...) {
    matrix <- build_matrix(matrix)
    dots <- rlang::list2(...)
    layers_idx <- vapply(dots, is_eheat_layer, logical(1L))
    layers <- dots[layers_idx]
    params <- dots[!layers_idx]
    methods::new("eheat",
        heatmap = rlang::inject(ComplexHeatmap::Heatmap(
            matrix = matrix,
            rect_gp = gpar(type = "none"),
            !!!params,
            layer_fun = NULL,
            show_heatmap_legend = FALSE
        )),
        layers = layers
    )
}

#' @importClassesFrom ComplexHeatmap Heatmap
methods::setClass(
    "eheat",
    slots = list(heatmap = "Heatmap", layers = "list")
)

#' @importFrom ComplexHeatmap draw
#' @export
#' @noRd
ComplexHeatmap::draw

methods::setMethod("draw", "eheat", function(object, ..., na.rm = FALSE) {
    # ComplexHeatmap::Heatmap will change the function environment of
    # `layer_fun`, we just assign it directly
    heat <- object@heatmap
    heat@matrix_param$layer_fun <- cheat_draw_fn(object, na.rm = na.rm)
    draw(heat, ...)
})
methods::setMethod("show", "eheat", function(object) {
    draw(object)
})

cheat_draw_fn <- function(object, na.rm = FALSE) {
    # as a termporary placeholder in order to only caculate these once
    slice <- NULL
    draw_fn_list <- NULL
    function(j, i, x, y, w, h, fill) {
        # https://github.com/jokergoo/ComplexHeatmap/blob/master/R/Heatmap-draw_component.R
        # trace back into `draw_heatmap_body()`
        env <- parent.frame()
        slice <<- slice %||% cheat_full_slice_index(list(
            row_order_list = env$object@row_order_list,
            column_order_list = env$object@column_order_list
        ))
        # we can also use grid::current.viewport()
        # and parse name to get kr or kc
        # -kr Row slice index.
        # -kc Column slice index.
        kr <- env$kr
        kc <- env$kc
        name <- sprintf("r%dc%d", kr, kc)
        draw_fn_list <<- draw_fn_list %||%
            eheat_build_draw_fn(object, slice, na.rm = na.rm)
        lapply(draw_fn_list, function(draw_fn) {
            draw_fn <- draw_fn[[name]]
            draw_fn(j, i, x, y, w, h, fill)
        })
    }
}

eheat_build_draw_fn <- function(object, slice, finite = FALSE, na.rm = FALSE) {
    if (is.null(object@layers)) {
        return(NULL)
    }
    heat_matrix <- object@heatmap@matrix
    .mapply(function(layer) {
        layer_matrix <- layer$layer_matrix(heat_matrix)

        # setup aesthetics to produce data with generalised variable names
        # and add default scales, will create a data.frame
        data <- layer$setup_aesthetics(layer_matrix)

        # Transform all scales
        scales <- layer$mapping$clone_scales()
        data <- scales$transform_df(data)

        # Do summary across row, column or slice
        data <- layer$compute_aesthetics(data, layer_matrix, slice)

        # correspoond to compute_geom_1
        # Reparameterise geoms from (e.g.) y and width to ymin and ymax
        data <- layer$compute_geom_1(data)
        # Train and map scales
        if (scales$n() > 0) {
            scales$train_df(data)
            data <- scales$map_df(data)
        }
        # Fill in defaults etc.
        data <- layer$compute_geom_2(data)
        layer$draw_fun(data, slice)
    }, list(layer = object@layers), NULL)
}

# Apply function to layer and matching data
by_layer <- function(f, layers, data, step = NULL) {
    ordinal <- scales::label_ordinal() # nolint
    out <- vector("list", length(data))
    rlang::try_fetch(
        for (i in seq_along(data)) {
            out[[i]] <- f(l = layers[[i]], d = data[[i]])
        },
        error = function(cnd) {
            cli::cli_abort(
                c("Problem while {step}.",
                    "i" = "Error occurred in the {ordinal(i)} layer."
                ),
                call = layers[[i]]$constructor, parent = cnd
            )
        }
    )
    out
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
