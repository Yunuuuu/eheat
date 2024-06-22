#' Create a heatmap annotation with ggplot object.
#' @inheritParams ggfit
#' @inheritParams new_anno
#' @inherit new_anno return
#' @examples
#' g <- ggplot(mpg, aes(displ, hwy, colour = class)) +
#'     geom_point()
#' m <- matrix(rnorm(100), 10)
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "panel",
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "plot",
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#' @seealso 
#' - [ggfit]
#' @export
anno_gg <- function(gg, align_with = "full", clip = NULL, gt = NULL,
                    width = NULL, height = NULL, show_name = FALSE,
                    which = NULL) {
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    force(gg)
    force(gt)
    new_anno(NA,
        function(index, k, n) {
            ggfit(gg,
                align_with = align_with, clip = clip,
                gt = gt
            )
        },
        which = which, width = width, height = height,
        show_name = show_name, name = "anno_gg"
    )
}

#' @seealso 
#' - [ggfit2]
#' @export
#' @rdname anno_gg
anno_gg2 <- function(gg, align_with = "full",
                     margins = c("b", "t", "l", "r"),
                     elements = c("axis", "lab", "guide"),
                     gt = NULL,
                     width = NULL, height = NULL, show_name = FALSE,
                     which = NULL) {
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    force(gg)
    force(margins)
    force(elements)
    force(gt)
    new_anno(NA,
        function(index, k, n) {
            ggfit2(gg,
                align_with = align_with,
                margins = margins,
                elements = elements,
                gt = gt
            )
        },
        which = which, width = width, height = height,
        show_name = show_name, name = "anno_gg"
    )
}
