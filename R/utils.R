`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

#' @importFrom grid gpar
#' @export
grid::gpar

#' @importFrom grid unit
#' @export
grid::unit

#' @importFrom ggplot2 alpha
#' @export
ggplot2::alpha

#' @importFrom ggplot2 .pt
#' @export
ggplot2::.pt

imap <- function(.x, .f, ...) {
    nms <- names(.x)
    out <- .mapply(.f, list(.x, nms %||% seq_along(.x)), NULL)
    if (!is.null(nms)) names(out) <- nms
    out
}

compact <- function(.x) {
    Filter(length, .x)
}

ggproto_formals <- function(x) {
    formals(environment(x)$f)
}

build_matrix <- function(matrix, arg = rlang::caller_arg(matrix)) {
    if (inherits(matrix, "data.frame")) {
        matrix <- as.matrix(matrix)
    } else if (!is.matrix(matrix)) {
        if (is.atomic(matrix)) {
            cli::cli_alert_info("convert simple vector {.arg matrix} to one-column matrix")
            matrix <- matrix(matrix, ncol = 1L)
        } else {
            cli::cli_abort("{.arg {arg}} must be a {.cls matrix}, a simple vector, or a {.cls data.frame}.")
        }
    }
    matrix
}

snake_class <- function(x) {
    snakeize(class(x)[1])
}

snakeize <- function(x) {
    x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
    x <- gsub(".", "_", x, fixed = TRUE)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    to_lower_ascii(x)
}
# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

pindex <- function(array, ...) {
    if (length(dim(array)) != ...length()) {
        stop("Indexing must have as many as the number of dimentions of array")
    }
    dots <- list(...)
    # all index must be atomic
    is_right <- vapply(dots, function(x) {
        is.atomic(x) && !is.null(x)
    }, logical(1L))
    if (!all(is_right)) {
        stop("All elements in ... must be atomic (`NULL` is not allowed)")
    }
    dots_len <- lengths(dots)
    if (any(dots_len == 0L)) {
        stop("Empty index is not allowed")
    }
    common_len <- max(dots_len)
    if (any(dots_len > 1L & dots_len < common_len)) {
        stop("Only index of length one are recycled")
    }
    if (common_len != 1L) {
        dots[dots_len == 1L] <- lapply(dots[dots_len == 1L], function(x) {
            rep_len(x, common_len)
        })
    }
    array[do.call("cbind", dots)]
}

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) {
        rlang::as_function(x)
    } else {
        x
    }
}

#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
rename <- function(x, replace) {
    current_names <- names(x)
    old_names <- names(replace)
    missing_names <- setdiff(old_names, current_names)
    if (length(missing_names) > 0) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

reverse_trans <- function(x) {
    sum(range(x, na.rm = TRUE)) - x
}

# Check required aesthetics are present
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @param character vector of required aesthetics
# @param character vector of present aesthetics
# @param name of object for error message
# @keyword internal
check_required_aesthetics <- function(required, present, name, call = rlang::caller_env()) {
    if (is.null(required)) {
        return()
    }

    required <- strsplit(required, "|", fixed = TRUE)
    if (any(lengths(required) > 1L)) {
        required <- lapply(required, rep_len, 2L)
        required <- list(
            vapply(required, `[`, character(1L), 1L),
            vapply(required, `[`, character(1L), 2L)
        )
    } else {
        required <- list(unlist(required, use.names = FALSE))
    }
    missing_aes <- lapply(required, setdiff, present)
    if (any(lengths(missing_aes) == 0L)) {
        return()
    }
    msg <- "{.fn {name}} requires the following missing aesthetics: {.field {missing_aes[[1]]}}"
    if (length(missing_aes) > 1) {
        msg <- paste0(msg, " {.strong or} {.field {missing_aes[[2]]}}")
    }
    cli::cli_abort(msg, call = call)
}

rename_aes <- function(x) {
    names(x) <- standardise_aes_names(names(x))
    duplicated_names <- names(x)[duplicated(names(x))]
    if (length(duplicated_names) > 0L) {
        cli::cli_warn("Duplicated aesthetics after name standardisation: {.field {unique0(duplicated_names)}}")
    }
    x
}

standardise_aes_names <- function(x) {
    x <- sub("color", "colour", x, fixed = TRUE)
    revalue(x, base_to_ggplot)
}

base_to_ggplot <- structure(
    c(
        "colour", "colour", "shape", "size", "linetype",
        "linewidth", "angle", "hjust", "fill", "colour", "ymin", "ymax"
    ),
    names = c("col", "color", "pch", "cex", "lty", "lwd", "srt", "adj", "bg", "fg", "min", "max")
)

revalue <- function(x, replace) {
    if (is.character(x)) {
        replace <- replace[names(replace) %in% x]
        if (length(replace) == 0) {
            return(x)
        }
        x[match(names(replace), x)] <- replace
    } else if (is.factor(x)) {
        lev <- levels(x)
        replace <- replace[names(replace) %in% lev]
        if (length(replace) == 0) {
            return(x)
        }
        lev[match(names(replace), lev)] <- replace
        levels(x) <- lev
    } else if (!is.null(x)) {
        stop_input_type(x, "a factor or character vector")
    }
    x
}

stop_input_type <- function(
    x, what,
    null_ok = FALSE,
    show_value = TRUE,
    show_length = FALSE,
    ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    if (null_ok) {
        what <- c(what, style_code("NULL"))
    }
    if (length(what)) {
        what <- oxford_comma(what, final = "or")
    }
    msg <- sprintf(
        "%s must be %s, not %s.",
        style_arg(arg),
        what,
        obj_type_friendly(x, value = show_value, length = show_length)
    )
    rlang::abort(msg, ..., call = call, arg = arg)
}

match_data <- function(data, i, j) {
    data[match(i * j, data$.idx), ]
}
