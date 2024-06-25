#' Prepare ExtendedHeatmap
#' @param object A [ExtendedHeatmap][eheat] or [ExtendedAnnotation][eanno]
#' object.
#' @param ... Additional arguments passed to specific methods
#' @return An modified [ExtendedHeatmap][eheat] or [ExtendedAnnotation][eanno]
#' object.
#' @examples 
#' eheat_prepare(eheat(matrix(rnorm(81), nrow = 9)))
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
