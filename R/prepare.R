#' Prepare ExtendedHeatmap
#' @param object A [ExtendedHeatmap][eheat] or [ExtendedAnnotation][eanno]
#' object.
#' @param ... Additional arguments passed to specific methods
#' @export
eheat_prepare <- function(object, ...) {
    UseMethod("eheat_prepare")
}

#' @export
#' @rdname eheat_prepare
eheat_prepare.ExtendedHeatmap <- function(object, ...) {
    object
}

#' @inheritParams eanno
#' @export
#' @rdname eheat_prepare
eheat_prepare.ExtendedAnnotation <- function(object, order_list, name = NULL,
                                             ...) {
    object
}
