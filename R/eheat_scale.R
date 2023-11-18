#' Wrap ggplot2 scale object into ComplexHeatmap
#' @param scale A [scale][ggplot2::discrete_scale] object.
#' @param matrix Matrix used for this aesthetic mapping
#' @export
eheat_scale <- function(scale, matrix = NULL) {
    if (!inherits(scale, "Scale")) {
        cli::cli_abort("{.arg scale} must be a {.cls Scale} object")
    }
    if (!is.null(matrix)) matrix <- build_matrix(matrix)
    ggplot2::ggproto("eheatScale", scale,
        matrix = matrix,
        # https://github.com/tidyverse/ggplot2/blob/main/R/scale-.R
        #  - `transform()` Transforms a vector of values using `self$trans`.
        #  This occurs before the `Stat` is calculated.
        map_aesthetic = function(self, layer_matrix, id) {
            right_dim <- dim(layer_matrix)
            if (!is.null(self$matrix) && !all(dim(self$matrix) == right_dim)) {
                msg <- "Scale matrix of {.field {id}} is not compatible with layer matrix"
                cli::cli_abort(msg, call = self$call)
            }
            aes_matrix <- self$matrix %||% layer_matrix
            values <- self$transform(aes_matrix)
            new_scale <- self$clone()
            new_scale$reset()
            new_scale$train(values)
            new_scale$map(values)
        }
    )
}

is_eheat_scale <- function(x) {
    inherits(x, "eheatScale")
}

wrap_eheat_scales <- function(..., call = rlang::caller_env()) {
    if (!...length()) {
        return(list())
    }
    if (is.list(..1)) {
        if (...length() == 1L) {
            out <- ..1
        } else {
            msg <- "You can only supply a single list or multiple {.cls eheatScale} objects into {.arg ...}"
            cli::cli_abort(msg, call = call)
        }
    } else {
        out <- list(...)
    }
    if (!all(vapply(out, is_eheat_scale, logical(1L)))) {
        msg <- "Only {.cls eheatScale} objects are allowed"
        cli::cli_abort(msg, call = call)
    }
    if (length(out) != sum(nzchar(names(out)))) {
        msg <- "All elements must be named"
        cli::cli_abort(msg, call = call)
    }
    out
}
