#' Legends from ggplot2
#' @param gg A [ggplot2][ggplot2::ggplot] object.
#' @return A list of [Legends][ComplexHeatmap::Legends] object.
#' @seealso ComplexHeatmap::Legend
#' @examples
#' gg <- ggplot(mtcars) +
#'     geom_bar(
#'         aes(mpg, disp, fill = factor(mpg)),
#'         stat = "identity"
#'     )
#' draw(guide_from_gg(gg)[[1L]])
#' @export
guide_from_gg <- function(gg) {
    guide_from_gtable(ggplot2::ggplotGrob(gg), direction = NULL)
}

guide_from_gtable <- function(gt, direction = NULL) {
    guides <- gtable::gtable_filter(gt, "guide-box")
    outs <- lapply(guides$grobs, function(x) {
        if (grid::is.grob(x) && inherits(x, "zeroGrob")) return(NULL) # styler: off
        guide <- gtable::gtable_filter(x, "guides")
        attr(guide, "width") <- gtable::gtable_width(guide)
        attr(guide, "height") <- gtable::gtable_height(guide)
        methods::new(
            "Legends",
            grob = guide,
            type = "gg_legend",
            name = "gg",
            n = 1L, multiple = 1L,
            # how to extract information directly from ggplot2?
            direction = match.arg(direction, c("vertical", "horizontal"))
        )
    })
    outs[!vapply(outs, is.null, logical(1L))]
}
