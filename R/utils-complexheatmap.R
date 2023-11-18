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
