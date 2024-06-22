#' Create a heatmap annotation with ggplot object.
#' @inheritParams ggfit
#' @inheritParams new_anno
#' @inherit new_anno return
#' @seealso
#' - [ggfit]
#' @examples
#' g <- ggplot(mpg, aes(displ, hwy, colour = class)) +
#'     geom_point()
#' m <- matrix(rnorm(100), 10)
#'
#' # anno_gg-panel: clip = "off" -------
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "panel",
#'             clip = "off",
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#'
#' # anno_gg-panel: clip = "on" --------
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "panel",
#'             clip = "on",
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#'
#' # anno_gg-plot ---------------------
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "plot",
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#'
#' # anno_gg-full --------------------
#' ggheat(m,
#'     top_annotation = HeatmapAnnotation(
#'         ggplot = anno_gg(g, "full",
#'             height = unit(6, "cm"),
#'             show_name = FALSE
#'         )
#'     )
#' )
#' @export
anno_gg <- function(gg, align_with = "full", clip = NULL, gt = NULL,
                    width = NULL, height = NULL, show_name = FALSE,
                    which = NULL) {
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    force(gg)
    force(gt)
    new_anno(NA,
        function(index, k, n) {
            ggfit(gg, align_with, clip, gt = gt)
        },
        which = which, width = width, height = height,
        show_name = show_name, name = "anno_gg"
    )
}

#' @seealso
#' - [ggfit2]
#' @examples
#' # anno_gg2-panel: margins = NULL -------
#' ggheat(m,
#'   top_annotation = HeatmapAnnotation(
#'     ggplot = anno_gg2(g, "panel",
#'       margins = NULL,
#'       height = unit(6, "cm"),
#'       show_name = FALSE
#'     )
#'   )
#' )
#' 
#' # anno_gg2-panel: margins = "l" --------
#' ggheat(m,
#'   top_annotation = HeatmapAnnotation(
#'     ggplot = anno_gg2(g, "panel",
#'       margins = "l",
#'       height = unit(6, "cm"),
#'       show_name = FALSE
#'     )
#'   )
#' )
#' 
#' # anno_gg2-panel: margins = "r" --------
#' ggheat(m,
#'   top_annotation = HeatmapAnnotation(
#'     ggplot = anno_gg2(g, "panel",
#'       margins = "r",
#'       height = unit(6, "cm"),
#'       show_name = FALSE
#'     )
#'   )
#' )
#' 
#' # anno_gg2-plot ---------------------
#' ggheat(m,
#'   top_annotation = HeatmapAnnotation(
#'     ggplot = anno_gg2(g, "plot",
#'       height = unit(6, "cm"),
#'       show_name = FALSE
#'     )
#'   )
#' )
#' 
#' # anno_gg2-full --------------------
#' ggheat(m,
#'   top_annotation = HeatmapAnnotation(
#'     ggplot = anno_gg2(g, "full",
#'       height = unit(6, "cm"),
#'       show_name = FALSE
#'     )
#'   )
#' )
#' @export
#' @rdname anno_gg
anno_gg2 <- function(gg, align_with = "full",
                     margins = c("b", "t", "l", "r"),
                     elements = c(
                         "axis", "lab", "guide",
                         "subtitle", "title", "caption"
                     ),
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
            ggfit2(gg, align_with, margins, elements, gt = gt)
        },
        which = which, width = width, height = height,
        show_name = show_name, name = "anno_gg2"
    )
}
