#' @inherit ComplexHeatmap::AnnotationFunction
#' @param draw_fn A function which defines how to draw the annotation. See
#' [ComplexHeatmap
#' Manual](https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#implement-new-annotation-functions)
#' for details.
#'
#' The function must have at least three arguments: `index`, `k` and `n` (the
#' names of the arguments can be arbitrary) where `k` and `n` are optional.
#' `index` corresponds to the indices of rows or columns of the heatmap. The
#' value of `index` is not necessarily to be the whole row indices or column
#' indices in the heatmap. It can also be a subset of the indices if the
#' annotation is split into slices according to the split of the heatmap.
#' `index` is reordered according to the reordering of heatmap rows or columns
#' (e.g. by clustering). So, `index` actually contains a list of row or column
#' indices for the current slice after row or column reordering.
#'
#' k corresponds to the current slice and n corresponds to the total number of
#' slices.
#'
#' You can always use `self` to indicates the matrix attached in this
#' annotation.
#'
#' @param ... Additional arguments passed on to `draw_fn`. Only named arguments
#' can be subsettable.
#' @param matrix A matrix, if it is a simple vector, it will be converted to a
#' one-column matrix. Data.frame will also be coerced into matrix. If `NULL`,
#' the matrix from heatmap will be used. You can also provide a function to
#' transform the matrix.
#' @inheritParams ComplexHeatmap::AnnotationFunction
#' @param subset_rule A list of function to subset variables in `...`.
#' @param fun_name Name of the annotation function, only used for message.
#' @param legends_margin,legends_panel A list of
#' [Legends][ComplexHeatmap::Legends-class] objects. `legends_margin` will be
#' added in the `annotation_legend_list` of
#' [draw][ComplexHeatmap::draw,HeatmapList-method]. `legends_panel` will be
#' plotted in the annotation panel. See [Legend][ComplexHeatmap::Legend] for
#' details. Only object with [make_legends] methods can be put in
#' `legends_margin`. Only object with [draw][draw-method] methods can be put in
#' `legends_panel`.
#' @details
#' `eanno` is similar with
#' [AnnotationFunction][ComplexHeatmap::AnnotationFunction], but `eanno` won't
#' change the function environment of `draw_fn`. So it's safe to use `eanno` in
#' pacakge development, particularly when dealing with internal functions in the
#' package namespace. In addition, all data has been attached in this object.
#' @examples
#' x <- 1:10
#' anno <- eanno(
#'     draw_fn = function(index, k, n) {
#'         n <- length(index)
#'         pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(0, 10)))
#'         grid.rect()
#'         grid.points(1:n, x[index], default.units = "native")
#'         if (k == 1) grid.yaxis()
#'         popViewport()
#'     },
#'     height = unit(2, "cm")
#' )
#' m <- rbind(1:10, 11:20)
#' eheat(m, top_annotation = HeatmapAnnotation(foo = anno))
#' eheat(m, top_annotation = HeatmapAnnotation(foo = anno), column_km = 2)
#'
#' anno <- eanno(
#'     function(index, k, n, self) {
#'         n <- length(index)
#'         pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(0, 10)))
#'         grid.rect()
#'         grid.points(1:n, self[index], default.units = "native")
#'         if (k == 1) grid.yaxis()
#'         popViewport()
#'     },
#'     matrix = rnorm(10L), subset_rule = TRUE,
#'     height = unit(2, "cm")
#' )
#' draw(anno)
#' draw(anno[1:2])
#' @seealso [AnnotationFunction][ComplexHeatmap::AnnotationFunction]
#' @return A `ExtendedAnnotation` object.
#' @importFrom ComplexHeatmap ht_opt
#' @export
eanno <- function(draw_fn, ..., matrix = NULL, which = NULL, subset_rule = NULL,
                  width = NULL, height = NULL, show_name = TRUE,
                  legends_margin = NULL, legends_panel = NULL,
                  fun_name = NULL) {
    if (ht_opt$verbose) {
        cli::cli_inform("construct ExtendedAnnotation with {.fn eanno}")
    }
    # ComplexHeatmap::AnnotationFunction() will change the function
    # environment of `anno@fun`
    # here: we use eanno instead, in this way, the function in the
    #       package namespace can be used directly
    draw_fn <- allow_lambda(draw_fn)
    assert_(draw_fn, is.function, "a function")
    matrix <- allow_lambda(matrix)
    if (is.null(matrix)) {
        n <- NA
    } else if (is.function(matrix)) {
        n <- NA
    } else {
        matrix <- build_matrix(matrix)
        n <- nrow(matrix)
    }
    which <- eheat_which(which)

    dots <- rlang::list2(...)

    # prepare subset rules ---------------------------------
    # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/AnnotationFunction-class.R#L270
    if (is.null(subset_rule)) {
        subsettable <- FALSE
        subset_rule <- list()
    } else if (is.logical(subset_rule)) {
        if (!is_scalar(subset_rule)) {
            cli::cli_abort("{.arg subset_rule} must be a single boolean value")
        } else if (is.na(subset_rule)) {
            cli::cli_abort("{.arg subset_rule} cannot be missing value")
        }

        if (subsettable <- subset_rule) {
            subset_rule <- lapply(
                dots[rlang::have_name(dots)],
                function(x) TRUE
            )
        }
    } else if (is.list(subset_rule)) {
        if (!rlang::is_named2(subset_rule)) {
            cli::cli_abort("{.arg subset_rule} must be named")
        }
        subset_rule <- lapply(subset_rule, allow_lambda)
        if (!all(vapply(subset_rule, is.function, logical(1L)))) {
            cli::cli_abort("{.arg subset_rule} must be a list of function")
        }
        subsettable <- TRUE
    }

    # contruct ExtendedAnnotation -----------------------------
    anno <- methods::new("ExtendedAnnotation")
    anno@dots <- dots
    anno@matrix <- matrix
    anno@which <- which
    anno@fun <- draw_fn
    anno@fun_name <- fun_name %||% "eanno"
    anno_size <- anno_width_and_height(which, width, height, unit(1, "cm"))
    anno@width <- anno_size$width
    anno@height <- anno_size$height
    anno@show_name <- show_name
    anno@n <- n
    anno@data_scale <- c(0L, 1L)
    anno@subsettable <- subsettable
    anno@subset_rule <- subset_rule

    # we change `var_env` into the environment of `@fun`
    anno@var_env <- environment(anno@fun)

    # assign legends ---------------------------
    if (is.null(legends_margin)) {
        legends_margin <- list()
    } else {
        legends_margin <- wrap_legend(legends_margin)
    }
    anno@legends_margin <- legends_margin
    if (is.null(legends_panel)) {
        legends_panel <- list()
    } else {
        legends_panel <- wrap_legend(legends_panel)
    }
    anno@legends_panel <- legends_panel
    anno
}

#' @export
`[.ExtendedAnnotation` <- function(x, i) {
    if (missing(i)) return(x) # styler: off
    if (!x@subsettable) {
        cli::cli_abort("{.arg x} is not subsettable.")
    }
    rules <- x@subset_rule
    x@dots[rlang::have_name(x@dots)] <- imap(
        x@dots[rlang::have_name(x@dots)], function(var, nm) {
            rule <- .subset2(rules, nm)
            if (is.null(rule) || isFALSE(rule)) {
                var
            } else if (isTRUE(rule)) {
                # subset element
                if (inherits(var, c("tbl_df", "data.table"))) {
                    var[i, ]
                } else if (is.matrix(var) || is.data.frame(var)) {
                    var[i, , drop = FALSE]
                } else if (inherits(var, "gpar")) {
                    subset_gp(var, i)
                } else if (is.vector(var) && !is_scalar(var)) {
                    .subset(var, i)
                }
            } else {
                rule(var, i)
            }
        }
    )
    if (is.matrix(x@matrix)) x@matrix <- x@matrix[i, , drop = FALSE]
    if (is_scalar(x@n) && is.na(x@n)) return(x) # styler: off
    if (is.logical(i)) {
        x@n <- sum(i)
    } else if (is.numeric(i)) {
        if (all(i > 0L)) x@n <- length(i)
        if (all(i < 0L)) x@n <- x@n - length(i)
    }
    return(x)
}

#' @importClassesFrom ComplexHeatmap AnnotationFunction
#' @export
#' @rdname eanno
methods::setClass(
    "ExtendedAnnotation",
    slots = list(
        matrix = "ANY",
        dots = "list",
        legends_margin = "list",
        legends_panel = "list",
        initialized = "logical"
    ),
    prototype = list(
        matrix = NULL,
        dots = list(),
        legends_margin = list(),
        legends_panel = list(),
        initialized = FALSE
    ),
    contains = "AnnotationFunction"
)

methods::setValidity("ExtendedAnnotation", function(object) {
    matrix <- object@matrix
    if (!is.null(matrix) && !is.function(matrix) && !is.matrix(matrix)) {
        cli::cli_abort("{.code @matrix} must be a matrix or a function or NULL")
    }
    TRUE
})

wrap_anno_fn <- function(fn, matrix, dots) {
    force(matrix)
    force(dots)

    # prepare annotation function --------------------------
    args <- formals(fn)
    # is.null is a fast path for a common case; the %in% check is slower but also
    # catches the case where there's a `self = NULL` argument.
    has_self <- !is.null(args[["self"]]) || "self" %in% names(args)
    if (has_self) {
        function(index, k, n) {
            rlang::inject(fn(index, k, n, !!!dots, self = matrix))
        }
    } else {
        function(index, k, n) {
            rlang::inject(fn(index, k, n, !!!dots))
        }
    }
}

#' @param object An [ExtendedAnnotation][eanno] object.
#' @param order_list Heatmap order list (column/row) after clustering.
#' @param heat_matrix Heatmap matrix.
#' @param name A string, the name of the annotation.
#' @importFrom ComplexHeatmap make_layout
#' @export
#' @rdname eanno
methods::setMethod(
    "make_layout", "ExtendedAnnotation",
    function(object, order_list, ..., heat_matrix = NULL, name = NULL) {
        which <- object@which
        # prepare ExtendedAnnotation matrix data ---------------------------
        mat <- object@matrix
        if (is.null(heat_matrix) && (is.null(mat) || is.function(mat))) {
            cli::cli_abort(paste(
                "You must provide a matrix in", object@fun_name,
                "in order to draw {.cls {fclass(object)}} directly"
            ))
        }
        if (is.null(mat)) {
            mat <- switch(which,
                row = heat_matrix,
                column = t(heat_matrix)
            )
            object@n <- nrow(mat)
        } else if (is.function(mat)) {
            data <- switch(which,
                row = heat_matrix,
                column = t(heat_matrix)
            )
            if (!is.matrix(mat <- mat(data))) {
                cli::cli_abort("{.fn @matrix} must return a matrix")
            }
            object@n <- nrow(mat)
        }
        object@matrix <- mat

        # call `eheat_prepare` to modify object after make_layout ----------
        object <- eheat_prepare(object, order_list, name)

        initialized_eanno_fn <- wrap_anno_fn(object@fun, mat, object@dots)

        if (is.null(name)) {
            vp_name <- NULL # should be called from draw directly
        } else {
            vp_name <- sprintf("annotation_%s", name)
        }
        dots <- rlang::list2(...)
        object@fun <- function(index, k, n) {
            # in the first slice, we always insert annotation viewport
            if (k == 1L) {
                if (!is.null(heat_matrix)) {
                    # current viewport: `draw,AnnotationFunction` function
                    # parent viewport - 1: `draw,HeatmapAnnotation` function
                    # parent viewport - 2: `draw,HeatmapAnnotation` function
                    # parent viewport - 3: `draw_annotation` function
                    #     for several annotation
                    # parent viewport - 4: `draw-internal` heatmap  function
                    # parent viewport - 5: `draw-internal` heatmap  function

                    # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/HeatmapList-draw_component.R#L668
                    # parent viewport - 6: `draw_heatmap_list` function

                    # parent viewport - 7: `draw_heatmap_list` function
                    #   -- "heatmap_{object@name}"
                    # parent viewport - 8: `draw_heatmap_list` function
                    #   -- "main_heatmap_list"
                    current_vp <- grid::current.viewport()$name
                    grid::upViewport(5L)
                    if (which == "row") {
                        yscale <- c(0.5, n + 0.5)
                        xscale <- c(0, 1)
                    } else {
                        xscale <- c(0.5, n + 0.5)
                        yscale <- c(0, 1)
                    }
                    vp <- grid::viewport(
                        x = dots$x, y = dots$y, width = dots$width,
                        height = dots$height, just = dots$just,
                        xscale = xscale, yscale = yscale,
                        name = vp_name
                    )
                    grid::pushViewport(vp)
                    grid::seekViewport(current_vp)
                }
            }
            initialized_eanno_fn(index, k, n)
            # .eheat_decorate(vp_name, {
            #     grid::grid.rect(gp = gpar(fill = NA, col = "red"))
            #     grid::grid.text(
            #         vp_name,
            #         x = unit(1, "mm"),
            #         y = unit(1, "npc") - unit(1, "mm"),
            #         just = c("left", "top"),
            #         gp = gpar(fontsize = 8)
            #     )
            # })
            # in the last slice, we draw legends in the panel
            if (k == n && length(object@legends_panel)) {
                if (is.null(vp_name)) {
                    lapply(object@legends_panel, draw)
                } else {
                    .eheat_decorate(vp_name, lapply(object@legends_panel, draw))
                }
            }
        }
        object@initialized <- TRUE
        object
    }
)

#' @param index A vector of indices.
#' @param k The current slice index for the annotation if it is split.
#' @param n Total number of slices.
#' @param ... Additional arguments passed on to
# [draw-AnnotationFunction][ComplexHeatmap::draw,HeatmapAnnotation-method].
#' @export
#' @importFrom ComplexHeatmap draw
#' @export
#' @rdname draw-method
methods::setMethod(
    "draw", "ExtendedAnnotation",
    definition = function(object, index, k = 1L, n = 1L, ...) {
        if (ht_opt$verbose) {
            cli::cli_inform("annotation generated by {.fn {object@fun_name}}")
        }
        if (missing(index)) {
            if (is.na(object@n)) {
                cli::cli_abort(
                    paste(
                        "You must provide {.arg index} to draw",
                        "{.cls {fclass(object)}} directly"
                    )
                )
            }
            index <- seq_len(object@n)
        }
        # since `eheat` will initialize `eanno` when preparing the main
        # heatmap layout. we skip this step.
        # This is only used by ComplexHeatmap::Heatmap function
        # since `eheat` will initialize `eanno` when preparing the main
        # heatmap layout.
        if (k == 1L && !object@initialized) {
            order_list <- NULL
            pos <- 1L
            nframes <- sys.nframe() - 1L # total parents
            # https://github.com/jokergoo/ComplexHeatmap/blob/7d95ca5cf533b98bd0351eecfc6805ad30c754c0/R/HeatmapList-draw_component.R#L670
            # trace back into `draw_heatmap_list()`
            # get slice informations from the draw function
            while (pos <= nframes) {
                env <- parent.frame(pos)
                if (is_from_eheat(env) &&
                    exists("ht_main", envir = env, inherits = FALSE) &&
                    is_call_from(pos, "draw_heatmap_list")) {
                    obj <- .subset2(env, "ht_main")
                    if (methods::.hasSlot(obj, "row_order_list") &&
                        methods::.hasSlot(obj, "column_order_list")) {
                        order_list <- switch(object@which,
                            row = obj@row_order_list,
                            column = obj@column_order_list
                        )
                    }
                }
                pos <- pos + 1L
            }
            if (is.null(order_list)) {
                if (n == 1L) {
                    order_list <- list(index)
                } else {
                    cli::cli_abort("Cannot initialize {.cls {object@name}}")
                }
            }
            # we can only supply heatmap matrix when called from `make_layout`
            # of heatmap, there is no heatmap matrix when called from draw
            # directly.
            object <- make_layout(object, order_list)
        }
        # will create a new viewport
        methods::callNextMethod(
            object = object, index = index,
            k = k, n = n, ...
        )
    }
)

anno_width_and_height <- function(which, width = NULL, height = NULL,
                                  default = unit(10, "mm")) {
    params <- list(width = width, height = height)
    if (which == "row") {
        # we flip the width and height in this way, both row and column
        # annotation, height must be absolute unit
        params <- flip_gp(params)
        arg <- "width" # nolint
    } else {
        arg <- "height"
    }
    if (is.null(.subset2(params, "height"))) {
        params$height <- default
    } else if (!ComplexHeatmap::is_abs_unit(.subset2(params, "height"))) {
        cli::cli_abort(paste(
            "{.arg {arg}} of the {.field {which}} annotation",
            "must be an absolute unit."
        ))
    }
    if (is.null(.subset2(params, "width"))) {
        params$width <- unit(1L, "npc")
    }
    if (which == "row") flip_gp(params) else params
}
