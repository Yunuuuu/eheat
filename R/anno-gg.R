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
#'             background = TRUE,
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "plot",
#'             background = TRUE,
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#' @seealso [ggfit]
#' @export
anno_gg <- function(gg, align = "panel", vp = NULL,
                    sides = c("b", "t", "l", "r"),
                    elements = c("axis", "lab", "guide"),
                    background = FALSE, gt = NULL,
                    width = NULL, height = NULL, show_name = FALSE,
                    which = NULL, name = NULL) {
    align <- match.arg(align, c("panel", "plot"))
    new_anno(NA,
        function(index, k, n) {
            ggfit(gg,
                align = align, sides = sides, elements = elements,
                background = background, gt = gt
            )
        },
        which = which, width = width, height = height,
        show_name = show_name, name = name %||% "anno_gg"
    )
}
