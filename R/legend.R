#' Legends from ggplot2
#' @param x A [ggplot2][ggplot2::ggplot] object.
#' @param margins Guides of which margin to draw besides the plot in the `vp`
#' viewport. Allowed values are `r rd_elements(c(MARGINS, "i"))`. "i" means the
#' inside legends (`ggplot2::theme(legend.position = "inside")`). Default:
#' `NULL`, which indicates `c("t", "l", "b", "r")`.
#' @return A list of [Legends][ComplexHeatmap::Legends-class] object.
#' @seealso [ComplexHeatmap::Legend]
#' @examples
#' gg <- ggplot(mtcars) +
#'     geom_bar(
#'         aes(mpg, disp, fill = factor(mpg)),
#'         stat = "identity"
#'     )
#' draw(make_legends(gg)[[1L]])
#' @export
make_legends <- function(x, margins = NULL) {
    lapply(get_guides(x, margins = margins), function(g) {
        attr(g, "width") <- gtable::gtable_width(g)
        attr(g, "height") <- gtable::gtable_height(g)
        methods::new(
            "Legends",
            grob = g,
            type = "gglegend",
            name = "gg",
            n = 1L, multiple = 1L,
            # how to extract information directly from ggplot2?
            direction = "vertical"
        )
    })
}

#' @keywords internal
get_guides <- function(x, ...) UseMethod("get_guides")

#' @export
get_guides.default <- function(x, ...) {
    cli::cli_abort("{.arg x} must be a {.cls ggplot} or a {.cls gtable} object")
}

#' @export
get_guides.ggplot <- function(x, ...) {
    get_guides(ggplot2::ggplotGrob(x), ...)
}

#' @export
get_guides.gtable <- function(x, margins = NULL, ...) {
    margins <- margins %||% MARGINS
    patterns <- gt_ggpatterns(margins, "guide")
    g <- gtable::gtable_filter(x, paste(patterns, collapse = "|"))$grobs
    # trim zeroGrobs
    g[!vapply(g, function(x) {
        grid::is.grob(x) && inherits(x, "zeroGrob")
    }, logical(1L))]
}
