#' Draw methods for additional objects
#'
#' @description Only object with [make_legends] methods can be put in
#' `legends_margin`. Only object with [draw][draw-method] methods can be put in
#' `legends_panel`.
#'
#' @param object See method signature
#' @param ... Additional argumentds passed on to specific methods.
#' @examples
#' draw(eheat(matrix(rnorm(81), nrow = 9)))
#' @importFrom ComplexHeatmap draw
#' @export
#' @name draw-method
ComplexHeatmap::draw

methods::setOldClass(c("gtable", "ggplot"))

#' @export
#' @rdname draw-method
methods::setMethod("draw", "gtable", function(object) grid::grid.draw(object))

#' @export
#' @rdname draw-method
methods::setMethod("draw", "ggplot", function(object) {
    object <- ggplot2::ggplotGrob(object)
    methods::callGeneric()
})
