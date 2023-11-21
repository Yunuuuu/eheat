#' @importFrom ComplexHeatmap ht_opt
#' @export
ComplexHeatmap::ht_opt

cheat_which <- function(which = NULL) {
    out <- cheat_env_get("current_annotation_which")
    if (is.null(out)) {
        out <- match.arg(which, c("column", "row"))
    }
    out
}

cheat_env_get <- function(name) {
    cheat_env()[[name]]
}

cheat_env <- function() {
    ComplexHeatmap:::.ENV
}

# get slice informations from the draw function
cheat_get_order_list <- function(name, pos = 2L, return_env = FALSE) {
    trace_data(
        name = name,
        has_fn = function(env, name) {
            exists(name, envir = env, inherits = FALSE) &&
                methods::.hasSlot(env[[name]], "row_order_list") &&
                methods::.hasSlot(env[[name]], "column_order_list")
        },
        return_fn = function(env, name) {
            list(
                row_order_list = env[[name]]@row_order_list,
                column_order_list = env[[name]]@column_order_list
            )
        },
        pos = pos, return_env = return_env
    )
}

cheat_full_slice_index <- function(slice) {
    out <- vector("list")
    for (i in seq_along(slice$row_order_list)) {
        row_order <- slice$row_order_list[[i]]
        nr <- length(row_order)
        for (j in seq_along(slice$column_order_list)) {
            column_order <- slice$column_order_list[[j]]
            nc <- length(column_order)
            expand_idx <- expand.grid(seq_len(nr), seq_len(nc))
            out[[sprintf("r%dc%d", i, j)]] <- list(
                row = row_order[expand_idx[[1]]],
                column = column_order[expand_idx[[2]]]
            )
        }
    }
    out
}

cheat_text_just <- function(rot, side) {
    rot <- rot %% 180
    if (side == "left") {
        if (rot == 0) {
            return(c(1, 0.5))
        } else if (rot == 90) {
            return(c(0.5, 0))
        } else if (rot == 270) {
            return(c(0.5, 1))
        }
    } else if (side == "right") {
        if (rot >= 0 && rot < 90) {
            return(c(0, 0.5))
        } else if (rot == 90) {
            return(c(0.5, 1))
        } else if (rot > 90 && rot < 180) {
            return(c(0, 0.5))
        }
    } else if (side == "top") {
        if (rot == 0) {
            return(c(0.5, 0))
        } else if (rot > 0 && rot <= 90) {
            return(c(0, 0.5))
        } else if (rot > 90 && rot <= 180) {
            return(c(1, 0.5))
        }
    } else if (side == "bottom") {
        if (rot == 0) {
            return(c(0.5, 1))
        } else if (rot > 0 && rot <= 90) {
            return(c(1, 0.5))
        } else if (rot > 90 && rot <= 180) {
            return(c(0, 0.5))
        }
    }
}

cheat_validate <- function(m, ha, which) {
    if (!ht_opt$validate_names) {
        return(NULL)
    }
    if (which == "column") {
        if (is.null(colnames(m))) {
            return(NULL)
        }
        cn <- colnames(m)
        for (i in seq_along(ha@anno_list)) {
            if (setequal(cn, names(ha@anno_list[[i]]))) {
                if (!identical(cn, names(ha@anno_list[[i]]))) {
                    cli::cli_warn("Values in column annotation '@{ha@anno_list[[i]]@name}' have a different order of names from the matrix column names. It may lead to wrong conclusion of your data. Please double check.")
                }
            }
        }
    }
    if (which == "row") {
        if (is.null(rownames(m))) {
            return(NULL)
        }
        rn <- rownames(m)
        for (i in seq_along(ha@anno_list)) {
            if (setequal(rn, names(ha@anno_list[[i]]))) {
                if (!identical(rn, names(ha@anno_list[[i]]))) {
                    cli::cli_warn("Values in row annotation '@{ha@anno_list[[i]]@name}' have a different order of names from the matrix row names. It may lead to wrong conclusion of your data. Please double check.")
                }
            }
        }
    }
}

cheat_check_gp <- function(gp) {
    if (!"lineheight" %in% names(gp)) {
        gp$lineheight <- 0.9
    }
    if (!inherits(gp, "gpar")) {
        cli::cli_abort("Graphic parameters should be specified by `gpar()`.")
    }
    return(gp)
}
