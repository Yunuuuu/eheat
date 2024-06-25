`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_nm <- function() utils::packageName(topenv(environment()))

null_paste <- function(..., sep = " ", collapse = NULL) {
    dots <- compact(list(...))
    do.call(paste, c(dots, list(sep = sep, collapse = collapse)))
}

is_scalar <- function(x) length(x) == 1L

#' @param n the number of generations to go back.
#' @param name function name.
#' @noRd
is_call_from <- function(n, call = NULL) {
    n <- n + 1L # `is_call_from` will generate one frame
    parent_call <- sys.call(sys.parent(n))
    call_nm <- rlang::call_name(parent_call)
    if (!is.call(call)) {
        parent_call <- call_nm
    }
    if (identical(parent_call, call)) return(TRUE) # styler: off
    # For S4 methods, the `call_nm` should be `.local`
    if (!identical(call_nm, ".local")) return(FALSE)  # styler: off
    parent_call <- sys.call(sys.parent(n + 1L))
    call_nm <- rlang::call_name(parent_call)
    if (!is.call(call)) {
        parent_call <- call_nm
    }
    identical(parent_call, call)
}

imap <- function(.x, .f, ...) {
    nms <- names(.x)
    out <- .mapply(.f, list(.x, nms %||% seq_along(.x)), NULL)
    if (!is.null(nms)) names(out) <- nms
    out
}

compact <- function(.x) .x[lengths(.x) > 0L]

build_matrix <- function(matrix, arg = rlang::caller_arg(matrix)) {
    if (inherits(matrix, "data.frame")) {
        matrix <- as.matrix(matrix)
    } else if (!is.matrix(matrix)) {
        if (is.atomic(matrix)) {
            cli::cli_alert_info("convert simple vector to one-column matrix")
            matrix <- matrix(matrix, ncol = 1L)
            colnames(matrix) <- "V1"
        } else {
            cli::cli_abort(paste(
                "{.arg {arg}} must be a {.cls matrix},",
                "a simple vector, or a {.cls data.frame}."
            ))
        }
    }
    matrix
}

pindex <- function(array, ...) {
    if (length(dim(array)) != ...length()) {
        stop("Indexing must have as many as the number of dimentions of array")
    }
    dots <- list(...)
    # all index must be atomic
    is_right <- vapply(
        dots, function(x) is.atomic(x) && !is.null(x),
        logical(1L)
    )
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

# List of all aesthetics known to ggplot
# (In the future, .all_aesthetics should be removed in favor
# of direct assignment to ggplot_global$all_aesthetics, see below.)
.all_aesthetics <- c(
    "adj", "alpha", "angle", "bg", "cex", "col", "color",
    "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
    "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
    "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
    "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z"
)

is_discrete <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
}

trace_data <- function(
    name,
    has_fn = function(env, name) {
        exists(name, envir = env, inherits = FALSE)
    },
    return_fn = function(env, name) {
        get(name, envir = env, inherits = FALSE)
    },
    pos = 2L, return_env = FALSE) {
    # the call of `trace_data` occupied one frame
    # we trace back caller environment
    n <- sys.nframe()
    while (pos <= n) {
        env <- parent.frame(pos)
        if (has_fn(env, name)) {
            out <- return_fn(env, name)
            if (return_env) {
                return(env)
            } else {
                return(out)
            }
        }
        pos <- pos + 1L
    }
    FALSE
}

as_tibble0 <- function(data, ...) {
    tibble::as_tibble(data, ..., .name_repair = "minimal")
}

data_frame0 <- function(...) {
    tibble::tibble(..., .name_repair = "minimal")
}

recycle_scalar <- function(x, length, arg = rlang::caller_arg(x)) {
    l <- length(x)
    if (l == 1L || l == length) {
        rep_len(x, length)
    } else {
        if (length != 1L) {
            msg <- sprintf("1 or %d", length) # nolint
        } else {
            msg <- "1"
        }
        cli::cli_abort("length of {.arg {arg}} can only be {msg}")
    }
}

scale_get_limits <- function(matrix, scale = NULL) {
    if (is.null(scale)) {
        if (is_discrete(matrix)) {
            scale <- ggplot2::scale_y_discrete()
        } else {
            scale <- ggplot2::scale_y_continuous()
        }
    }
    new_scale <- scale$clone()
    new_scale$reset()
    new_scale$train(matrix)
    new_scale$get_limits()
}

fclass <- function(x) .subset(class(x), 1L)
